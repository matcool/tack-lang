use std::{
	cell::RefCell,
	path::{Path, PathBuf},
	rc::Rc,
};

use crate::{
	ast::{
		BuiltInType, Expression, ExpressionKind, Function, HasAST, Scope, StructType, Type,
		TypeRef, Variable, AST, BUILTIN_TYPE_INT_LITERAL,
	},
	diagnostics::{ErrorBuilder, ProducesError},
	lexer::Lexer,
	location,
	parser::{self, Parser},
};

mod expressions;
mod statements;

pub struct TypeChecker {
	pub ast: AST,
	file_path: PathBuf,
	has_errored: RefCell<bool>,
}

struct FunctionTypeChecker<'a> {
	ast: &'a mut AST,
	function: &'a Function,
	scope: Rc<Scope>,
	file_path: &'a Path,
	has_errored: &'a RefCell<bool>,
}

impl<T: ProducesError + HasAST> ErrorBuilder<'_, T> {
	fn build_type_mismatch(self, actual: TypeRef, expected: TypeRef) -> TypeRef {
		let description = format!(
			"Expected {}, got {}",
			expected.remove_reference().formatted(self.this.ast()),
			actual.remove_reference().formatted(self.this.ast())
		);
		self.description(description).build();
		expected
	}
	fn build_expected_reference(self) {
		self.description("Expected reference").build();
	}
	fn build_variable_not_found(self, name: &str) {
		self.message(format!("Variable \"{name}\" not found"))
			.build();
	}
}

fn dummy_expr() -> Expression {
	Expression::new(TypeRef::unknown(), ExpressionKind::NumberLiteral(0))
}

impl TypeChecker {
	pub fn new(file_path: PathBuf) -> TypeChecker {
		TypeChecker {
			ast: AST::new(file_path.clone()),
			file_path,
			has_errored: false.into(),
		}
	}

	pub fn check(mut self, parser: Parser) -> Result<Vec<AST>, ()> {
		let mut asts = Vec::new();

		// check imported files
		for imported_file in &parser.imported_files {
			let imported_path = self
				.file_path
				.parent()
				.unwrap_or_else(|| panic!("invalid path?"))
				.join(imported_file);

			let Ok(contents) = std::fs::read_to_string(imported_path.clone()) else {
				self.error(Default::default(), location!())
					.message(format!("Imported file {imported_file} could not be found"))
					.build();
				continue;
			};

			let mut lexer = Lexer::new(contents.chars().peekable());
			let tokens: Vec<_> = lexer.iter().collect();

			let mut parser = Parser::new(tokens.into_iter().peekable(), imported_path.clone());
			parser.parse().unwrap();

			let checker = TypeChecker::new(imported_path);
			let i = asts.len();
			asts.extend(checker.check(parser).unwrap());
			let new_ast = &asts[i];

			let clone_type = |new_ast: &mut AST, old_ast: &AST, type_ref: TypeRef| {
				new_ast.find_type_or_add(old_ast.get_type(type_ref).clone())
			};

			for function in &new_ast.functions {
				if function.is_external() {
					continue;
				}
				let mut imported_func = function.clone();
				imported_func.attributes.is_extern = true;
				for arg in &mut imported_func.arguments {
					arg.ty = clone_type(&mut self.ast, new_ast, arg.ty);
				}
				imported_func.return_type =
					clone_type(&mut self.ast, new_ast, imported_func.return_type);
				self.ast.functions.push(imported_func);
			}

			// adds structs that arent mentioned in functions
			for ty in &new_ast.types {
				if self.ast.find_type(|t| t == &ty).is_none() {
					self.ast.add_type(ty.clone());
				}
			}
		}

		// check structs
		for parsed_struct in parser.parsed_structs {
			let mut fields: Vec<Variable> = vec![];
			for field in parsed_struct.fields {
				if let Some(existing_field) = fields.iter().find(|f| f.name == field.name) {
					// TODO: show the other one
					self.error(field.span, location!())
						.message("Duplicate field names")
						.description(format!("Re-definition of \"{}\"", field.name))
						.extra(existing_field.span, "First declared here")
						.build();
					continue;
				}
				fields.push(self.ast.check_parsed_var(field));
			}
			self.ast.add_type(Type::Struct(StructType {
				name: parsed_struct.name.clone(),
				fields,
			}));
		}

		// check functions
		for function in parser.functions {
			let function = self.check_function(function);
			self.ast.functions.push(function);
		}

		asts.insert(0, self.ast);

		if self.has_errored.into_inner() {
			std::process::exit(1);
		}
		Ok(asts)
	}

	fn check_function(&mut self, parsed: parser::Function) -> Function {
		let return_type = self.ast.check_parsed_type(parsed.return_type);
		let mut arguments = vec![];
		for arg in parsed.arguments {
			arguments.push(self.ast.check_parsed_var(arg));
		}

		let mut function = Function::new(parsed.name);
		function.return_type = return_type;
		function.arguments = arguments;
		function.attributes = parsed.attributes;
		if !function.attributes.is_c_extern {
			let scope = self.check_function_scope(parsed.scope, &mut function);
			function.scope = scope;
		}

		function
	}

	fn check_function_scope(
		&mut self,
		parsed: parser::Scope,
		function: &mut Function,
	) -> Rc<Scope> {
		let scope = Rc::new(Scope::new(None));
		// add the function args
		for arg in &mut function.arguments {
			*arg = scope.add_variable(arg.clone());
		}
		let mut checker = FunctionTypeChecker {
			ast: &mut self.ast,
			function,
			scope: Rc::clone(&scope),
			file_path: &self.file_path,
			has_errored: &self.has_errored,
		};
		for statement in parsed.statements {
			let stmt = checker.check_statement(statement);
			scope.add_statement(stmt);
		}
		scope
	}
}

impl AST {
	fn check_parsed_type(&mut self, parsed: parser::Type) -> TypeRef {
		match parsed {
			parser::Type::Name(name) => self.find_type_by_name(&name).unwrap_or_else(|| {
				todo!("error here");
			}),
			parser::Type::Pointer(inner) => {
				let inner = self.check_parsed_type(*inner);
				self.find_type_or_add(Type::Pointer(inner))
			}
			parser::Type::Array(inner, size) => {
				let inner = self.check_parsed_type(*inner);
				self.find_type_or_add(Type::Array(inner, size))
			}
			parser::Type::Unknown => unreachable!(),
		}
	}

	fn check_parsed_var(&mut self, parsed: parser::Variable) -> Variable {
		Variable {
			name: parsed.name.clone(),
			unique_id: 0,
			ty: self.check_parsed_type(parsed.ty),
			span: parsed.span,
		}
	}
}

impl FunctionTypeChecker<'_> {
	fn check_scope(&mut self, parsed: parser::Scope) -> Rc<Scope> {
		let scope = Rc::new(Scope::new(Some(Rc::clone(&self.scope))));
		let mut checker = FunctionTypeChecker {
			ast: self.ast,
			function: self.function,
			scope: Rc::clone(&scope),
			file_path: self.file_path,
			has_errored: self.has_errored,
		};
		for statement in parsed.statements {
			let stmt = checker.check_statement(statement);
			scope.add_statement(stmt);
		}
		scope
	}

	fn promote_int_literal_into(&self, expression: &mut Expression, type_ref: TypeRef) -> TypeRef {
		if expression.value_type != BUILTIN_TYPE_INT_LITERAL {
			return expression.value_type;
		}
		let target_type = self.ast.get_type(type_ref);
		if let Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8 | BuiltInType::UPtr) = target_type {
			expression.value_type = type_ref.remove_reference();
			// TODO: children trait or something
			if let ExpressionKind::BinaryOperator(_, left, right) = &mut expression.kind {
				self.promote_int_literal_into(left, type_ref);
				self.promote_int_literal_into(right, type_ref);
			} else if let ExpressionKind::UnaryOperator(_, left) = &mut expression.kind {
				self.promote_int_literal_into(left, type_ref);
			} else {
				// todo!("Tried to promote something else {}", expression.kind);
			}
		}
		expression.value_type
	}

	fn find_variable(&self, name: &str) -> Option<Variable> {
		if let Some(var) = self.scope.find_variable_recursive(name) {
			return Some(var);
		}
		self.function
			.arguments
			.iter()
			.find(|var| var.name == name)
			.cloned()
	}
}

impl ProducesError for TypeChecker {
	fn file_path(&self) -> PathBuf {
		self.file_path.clone()
	}
	fn set_errored(&self) {
		self.has_errored.replace(true);
	}
}

impl ProducesError for FunctionTypeChecker<'_> {
	fn file_path(&self) -> PathBuf {
		self.file_path.to_owned()
	}
	fn set_errored(&self) {
		self.has_errored.replace(true);
	}
}

impl HasAST for TypeChecker {
	fn ast(&self) -> &AST {
		&self.ast
	}
}

impl HasAST for FunctionTypeChecker<'_> {
	fn ast(&self) -> &AST {
		self.ast
	}
}
