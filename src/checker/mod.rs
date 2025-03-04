use std::{
	cell::RefCell,
	path::{Path, PathBuf},
	rc::Rc,
};

use crate::{
	ast::{
		BuiltInType, Expression, ExpressionKind, Function, HasAST, Namespace, Scope, StructType,
		Type, TypeRef, Variable, AST, BUILTIN_TYPE_INT_LITERAL,
	},
	diagnostics::{ErrorBuilder, ProducesError},
	lexer::Lexer,
	location,
	parser::{self, Parser},
	span::Span,
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
	fn build_unknown_struct_field(self, unk_field: &str, struct_type: &StructType) {
		// TODO: maybe try to find closest match?
		self.message(format!(
			"No field \"{unk_field}\" in struct {}",
			struct_type.name
		))
		.description(format!("Unknown field {unk_field}"))
		.build();
	}
}

fn dummy_expr(span: Span) -> Expression {
	Expression::new_spanned(TypeRef::unknown(), ExpressionKind::NumberLiteral(0), span)
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
				.expect("invalid path?")
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
			let old_ast = &asts[i];

			self.ast.import_ast(old_ast);
		}

		let mut pending_functions = Vec::new();

		// check structs
		for parsed_struct in parser.parsed_structs {
			let mut fields: Vec<Variable> = vec![];
			for field in parsed_struct.fields {
				if let Some(existing_field) = fields.iter().find(|f| f.name == field.name) {
					self.error(field.span, location!())
						.message("Duplicate field names")
						.description(format!("Re-definition of \"{}\"", field.name))
						.extra(existing_field.span, "First declared here")
						.build();
					continue;
				}
				fields.push(self.ast.check_parsed_var(field));
			}
			let struct_type_ref = self.ast.add_type(Type::Struct(StructType {
				name: parsed_struct.name.clone(),
				fields,
			}));
			let struct_namespace = self
				.ast
				.add_namespace(self.ast.global, Namespace::new(parsed_struct.name));
			for function in parsed_struct.functions {
				let (function, scope) = self.check_struct_function(function, struct_type_ref);
				let key = self.ast.add_function(struct_namespace, function);
				pending_functions.push((key, scope));
			}
		}

		// check functions
		for function in parser.functions {
			let (function, scope) = self.check_function(function);
			let key = self.ast.add_function(self.ast.global, function);
			pending_functions.push((key, scope));
		}

		// actually check function scopes
		for (key, scope) in pending_functions {
			// cloning here is probably not necessary but oh well!
			let function = self.ast.functions.get(key).unwrap().clone();

			if !function.attributes.is_c_extern {
				let function = self.check_function_scope(scope, function);
				self.ast.functions[key] = function;
			}
		}

		if self.has_errored.into_inner() {
			std::process::exit(1);
		}

		asts.insert(0, self.ast);
		Ok(asts)
	}

	fn check_function(&mut self, parsed: parser::Function) -> (Function, parser::Scope) {
		let return_type = self.ast.check_parsed_type(parsed.return_type);
		let mut arguments = vec![];
		for arg in parsed.arguments {
			arguments.push(self.ast.check_parsed_var(arg));
		}

		let mut function = Function::new(parsed.name);
		function.return_type = return_type;
		function.arguments = arguments;
		function.attributes = parsed.attributes;

		(function, parsed.scope)
	}

	fn check_struct_function(
		&mut self,
		parsed: parser::Function,
		struct_type: TypeRef,
	) -> (Function, parser::Scope) {
		let return_type = self.ast.check_parsed_type(parsed.return_type);
		let mut arguments = vec![];
		for arg in parsed.arguments {
			// TODO: hack
			if arg.name == "self" {
				arguments.push(Variable {
					name: "self".into(),
					unique_id: 0,
					ty: self.ast.find_type_or_add(Type::Pointer(struct_type)),
					span: arg.span,
				});
			} else {
				arguments.push(self.ast.check_parsed_var(arg));
			}
		}

		let mut function = Function::new(parsed.name);
		function.return_type = return_type;
		function.arguments = arguments;
		function.attributes = parsed.attributes;

		(function, parsed.scope)
	}

	fn check_function_scope(&mut self, parsed: parser::Scope, mut function: Function) -> Function {
		let scope = Rc::new(Scope::new(None));
		// add the function args
		for arg in &mut function.arguments {
			*arg = scope.add_variable(arg.clone());
		}
		let mut checker = FunctionTypeChecker {
			ast: &mut self.ast,
			function: &function,
			scope: Rc::clone(&scope),
			file_path: &self.file_path,
			has_errored: &self.has_errored,
		};
		for statement in parsed.statements {
			let stmt = checker.check_statement(statement);
			scope.add_statement(stmt);
		}
		function.scope = scope;
		function
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
		if expression.ty != BUILTIN_TYPE_INT_LITERAL {
			return expression.ty;
		}
		let target_type = self.ast.get_type(type_ref);
		if let Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8 | BuiltInType::UPtr) = target_type {
			expression.ty = type_ref.remove_reference();
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
		expression.ty
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
