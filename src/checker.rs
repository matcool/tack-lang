use std::{
	cell::RefCell,
	path::{Path, PathBuf},
	rc::Rc,
};

use itertools::Itertools;

use crate::{
	ast::{
		BuiltInType, Expression, ExpressionKind, Function, Scope, Statement, StatementKind,
		StructType, Type, TypeRef, Variable, AST, BUILTIN_TYPE_BOOL, BUILTIN_TYPE_I32,
		BUILTIN_TYPE_INT_LITERAL, BUILTIN_TYPE_STR, BUILTIN_TYPE_VOID,
	},
	diagnostics::{ErrorBuilder, ProducesError},
	lexer::Operator,
	location,
	parser::{self, Parser},
};

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

impl<T: ProducesErrorWithAST> ErrorBuilder<'_, T> {
	fn build_type_mismatch(self, actual: TypeRef, expected: TypeRef) -> TypeRef {
		let description = format!(
			"Expected {}, got {}",
			expected.remove_reference().formatted(self.this.get_ast()),
			actual.remove_reference().formatted(self.this.get_ast())
		);
		self.description(description).build();
		expected
	}
	fn build_expected_reference(self) {
		self.description("Expected reference").build();
	}
	fn build_variable_not_found(self, name: &str) {
		self.message(format!("Variable {name} not found")).build();
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

		for parsed_struct in parser.parsed_structs {
			let mut fields: Vec<Variable> = vec![];
			for field in parsed_struct.fields {
				if fields.iter().any(|f| f.name == field.name) {
					// TODO: show the other one
					self.error(Default::default(), location!())
						.message("Duplicate field names")
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

	fn check_statement(&mut self, parsed: parser::Statement) -> Statement {
		match parsed.kind {
			parser::StatementKind::Expression(expr) => {
				let expr = self.check_expression(expr);
				Statement::new(StatementKind::Expression(expr), parsed.span)
			}
			parser::StatementKind::Return(expr_opt) => {
				if self.function.return_type == BUILTIN_TYPE_VOID {
					if let Some(expr) = expr_opt {
						// TODO: should prob allow it as long as u cast to void
						// or maybe allow implicit casting to void
						self.error(expr.span, location!())
							.message("Unexpected return value in void function")
							.build();
					}
					Statement::new(StatementKind::Return(None), parsed.span)
				} else {
					let Some(expr) = expr_opt else {
						self.error(parsed.span, location!())
							.message("Expect return value")
							.build();
						// TODO: maybe some dummy statement kind for this?
						return Statement::new(StatementKind::Return(None), parsed.span);
					};
					let mut expr = self.check_expression(expr);
					let ty = self.promote_int_literal_into(&mut expr, self.function.return_type);
					if ty != self.function.return_type {
						self.error(expr.span, location!())
							.message("Expression does not match return type")
							.build_type_mismatch(expr.value_type, self.function.return_type);
					}
					expr.cast_if_reference();
					Statement::new(StatementKind::Return(Some(expr)), parsed.span)
				}
			}
			parser::StatementKind::If(parsed_scope, condition, else_stmt) => {
				let mut condition = self.check_expression(condition);
				condition.cast_if_reference();
				if condition.value_type != BUILTIN_TYPE_BOOL {
					self.error(condition.span, location!())
						.message("Condition must be boolean")
						.build_type_mismatch(condition.value_type, BUILTIN_TYPE_BOOL);
				}
				let if_scope = self.check_scope(parsed_scope);
				let else_stmt = else_stmt.map(|stmt| Box::new(self.check_statement(*stmt)));

				Statement::new(
					StatementKind::If(if_scope, condition, else_stmt),
					parsed.span,
				)
			}
			parser::StatementKind::Block(parsed_scope) => {
				let scope = self.check_scope(parsed_scope);
				Statement::new(StatementKind::Block(scope), parsed.span)
			}
			parser::StatementKind::While(parsed_scope, condition) => {
				let mut condition = self.check_expression(condition);
				condition.cast_if_reference();
				if condition.value_type != BUILTIN_TYPE_BOOL {
					self.error(condition.span, location!())
						.message("Condition must be boolean")
						.build_type_mismatch(condition.value_type, BUILTIN_TYPE_BOOL);
				}
				let scope = self.check_scope(parsed_scope);
				Statement::new(StatementKind::While(scope, condition), parsed.span)
			}
		}
	}

	fn check_expression(&mut self, parsed: parser::Expression) -> Expression {
		match parsed.kind {
			parser::ExpressionKind::NumberLiteral(value) => Expression::new_spanned(
				BUILTIN_TYPE_INT_LITERAL,
				ExpressionKind::NumberLiteral(value),
				parsed.span,
			),
			parser::ExpressionKind::BoolLiteral(value) => Expression::new_spanned(
				BUILTIN_TYPE_BOOL,
				ExpressionKind::BoolLiteral(value),
				parsed.span,
			),
			parser::ExpressionKind::StringLiteral(value) => Expression::new_spanned(
				BUILTIN_TYPE_STR,
				ExpressionKind::StringLiteral(value),
				parsed.span,
			),
			parser::ExpressionKind::BinaryOperator(Operator::Assign, left, right) => {
				// check rhs first
				let mut right = self.check_expression(*right);
				let left = self.check_expression(*left);
				right.cast_if_reference();

				let left_ty = left.value_type;
				if !left_ty.reference {
					// this should prob not be a type mismatch
					self.error(left.span, location!())
						.message("Left side must be ref-able expression")
						.build_expected_reference();
				}

				// Promote rhs into lhs if possible
				let right_ty = self.promote_int_literal_into(&mut right, left_ty);

				if left_ty != right_ty {
					self.error(right.span, location!())
						.message("Left side and right side types don't match")
						.build_type_mismatch(right_ty, left_ty);
				}

				Expression::new_spanned(
					BUILTIN_TYPE_VOID,
					ExpressionKind::BinaryOperator(Operator::Assign, left.into(), right.into()),
					parsed.span,
				)
			}
			parser::ExpressionKind::BinaryOperator(op, left, right) => {
				let mut left = self.check_expression(*left);
				let mut right = self.check_expression(*right);

				// Promote rhs into lhs if possible
				let right_ty = self.promote_int_literal_into(&mut right, left.value_type);
				// Otherwise, promote lhs into rhs
				let left_ty = self.promote_int_literal_into(&mut left, right_ty);

				// Boolean operators
				if matches!(op, Operator::And | Operator::Or)
					&& (left_ty != BUILTIN_TYPE_BOOL || right_ty != BUILTIN_TYPE_BOOL)
				{
					let which = if left_ty != BUILTIN_TYPE_BOOL {
						(left.span, left_ty)
					} else {
						(right.span, right_ty)
					};
					self.error(which.0, location!())
						.message("Both operands must be bool")
						.build_type_mismatch(which.1, BUILTIN_TYPE_BOOL);
				}

				// Pointer arithmetic
				if self.ast.is_pointer(left_ty) {
					let rhs = self.promote_int_literal_into(&mut right, BUILTIN_TYPE_I32);
					if !matches!(op, Operator::Add | Operator::Sub) {
						self.error(parsed.span, location!())
							.message("Pointers only support addition and subtraction")
							.build();
					}
					if rhs != BUILTIN_TYPE_I32 {
						self.error(right.span, location!())
							.message("Pointer arithmetic must be done with i32")
							.build_type_mismatch(right.value_type, BUILTIN_TYPE_I32);
					}
					left.cast_if_reference();
					right.cast_if_reference();
					return Expression::new_spanned(
						left_ty.remove_reference(),
						ExpressionKind::BinaryOperator(op, left.into(), right.into()),
						parsed.span,
					);
				}

				if left_ty != right_ty {
					self.error(right.span, location!())
						.message("Operand types don't match")
						.build_type_mismatch(right_ty, left_ty);
				}

				if matches!(
					op,
					Operator::Add | Operator::Sub | Operator::Divide | Operator::Multiply
				) {
					let ty = self.ast.get_type(left_ty);
					if !matches!(
						ty,
						Type::BuiltIn(
							BuiltInType::I32
								| BuiltInType::UPtr | BuiltInType::U8
								| BuiltInType::IntLiteral
						)
					) {
						self.error(parsed.span, location!())
							.message("Arithmetic can only be done on integer types")
							.build_type_mismatch(left_ty, BUILTIN_TYPE_INT_LITERAL);
					}
				}

				left.cast_if_reference();
				right.cast_if_reference();

				let ty = match op {
					Operator::Equals
					| Operator::NotEquals
					| Operator::GreaterThan
					| Operator::GreaterThanEq
					| Operator::LessThan
					| Operator::LessThanEq => BUILTIN_TYPE_BOOL,
					_ => left_ty.remove_reference(),
				};
				Expression::new_spanned(
					ty,
					ExpressionKind::BinaryOperator(op, left.into(), right.into()),
					parsed.span,
				)
			}
			parser::ExpressionKind::UnaryOperator(Operator::Negate, child) => {
				let mut child = self.check_expression(*child);
				if child.value_type != BUILTIN_TYPE_INT_LITERAL
					&& !self.ast.is_integer(child.value_type)
				{
					self.error(parsed.span, location!())
						.message("Negation must be done on integers")
						.build_type_mismatch(child.value_type, BUILTIN_TYPE_INT_LITERAL);
				}
				child.cast_if_reference();
				Expression::new_spanned(
					child.value_type,
					ExpressionKind::UnaryOperator(Operator::Negate, child.into()),
					parsed.span,
				)
			}
			parser::ExpressionKind::Declaration(parsed_var) => {
				let var = self
					.scope
					.add_variable(self.ast.check_parsed_var(parsed_var));
				Expression::new_spanned(
					var.ty.add_reference(),
					ExpressionKind::Declaration(var),
					parsed.span,
				)
			}
			parser::ExpressionKind::Identifier(ref name) => {
				let Some(var) = self.find_variable(name) else {
					self.error(parsed.span, location!())
						.build_variable_not_found(name);
					return dummy_expr();
				};
				Expression::new_spanned(
					var.ty.add_reference(),
					ExpressionKind::Identifier(var),
					parsed.span,
				)
			}
			parser::ExpressionKind::StructAccess(struct_expr, field_name) => {
				let mut struct_expr = self.check_expression(*struct_expr);
				let struct_ty: Option<TypeRef>;
				match self.ast.get_type(struct_expr.value_type) {
					Type::Struct(_) => {
						struct_ty = Some(struct_expr.value_type);
					}
					Type::Pointer(inner) => {
						if self.ast.is_struct(*inner) {
							struct_ty = Some(*inner);
						} else {
							struct_ty = None;
						}
					}
					_ => struct_ty = None,
				}
				let Some(struct_ty) = struct_ty else {
					self.error(parsed.span, location!())
						.message("Expected struct")
						.build();
					return dummy_expr();
				};
				if self.ast.is_pointer(struct_expr.value_type) {
					struct_expr.cast_if_reference();
					struct_expr = Expression::new(
						struct_ty.add_reference(),
						ExpressionKind::UnaryOperator(Operator::Dereference, struct_expr.into()),
					);
				}
				let Type::Struct(struct_ty) = self.ast.get_type(struct_ty) else {
					unreachable!()
				};
				if let Some(field) = struct_ty.fields.iter().find(|f| f.name == field_name) {
					let ty = if struct_expr.value_type.reference {
						field.ty.add_reference()
					} else {
						field.ty
					};

					Expression::new_spanned(
						ty,
						ExpressionKind::StructAccess(struct_expr.into(), field_name),
						parsed.span,
					)
				} else {
					self.error(parsed.span, location!())
						.message(format!(
							"No field {field_name} in struct {}",
							struct_ty.name
						))
						.description(format!("Unknown field {field_name}"))
						.build();
					dummy_expr()
				}
			}
			parser::ExpressionKind::ArrayLiteral(values) => {
				let mut values = values
					.into_iter()
					.map(|e| self.check_expression(e))
					.collect_vec();

				let mut inner_type = TypeRef::unknown();
				for child in &mut values {
					child.cast_if_reference();
					if inner_type.is_unknown() {
						inner_type = child.value_type;
					} else {
						self.promote_int_literal_into(child, inner_type);
						if inner_type != child.value_type {
							self.error(parsed.span, location!())
								.message(format!(
									"Array expected {}, got {}",
									self.format_type(inner_type),
									self.format_type(child.value_type)
								))
								.build_type_mismatch(child.value_type, inner_type);
						}
					}
				}
				if inner_type.is_unknown() {
					self.error(parsed.span, location!())
						.message("Could not determine type of array")
						.build();
				}
				let ty = self
					.ast
					.find_type_or_add(Type::Array(inner_type, values.len()));
				Expression::new_spanned(ty, ExpressionKind::ArrayLiteral(values), parsed.span)
			}
			parser::ExpressionKind::ArrayIndex(arr_expr, index_expr) => {
				let mut index_expr = self.check_expression(*index_expr);
				index_expr.cast_if_reference();
				let index_type = self.promote_int_literal_into(&mut index_expr, BUILTIN_TYPE_I32);
				if !self.ast.is_integer(index_type) {
					self.error(parsed.span, location!())
						.message("Arrays must be indexed with integers")
						.build_type_mismatch(index_type, BUILTIN_TYPE_INT_LITERAL);
				}

				let mut arr_expr = self.check_expression(*arr_expr);
				arr_expr.cast_if_reference();
				let arr_type = arr_expr.value_type;

				let ty = if let Type::Pointer(inner) = self.ast.get_type(arr_type) {
					inner.add_reference()
				} else if let Type::Array(inner, _) = self.ast.get_type(arr_type) {
					inner.add_reference()
				} else {
					self.error(parsed.span, location!())
						.message("Indexing is only supported on array and pointers")
						.build();
					TypeRef::unknown()
				};

				Expression::new_spanned(
					ty,
					ExpressionKind::ArrayIndex(arr_expr.into(), index_expr.into()),
					parsed.span,
				)
			}
			parser::ExpressionKind::Cast(into, child) => {
				let into = self.ast.check_parsed_type(into);
				let mut child = self.check_expression(*child);
				let from = child.value_type;

				match (self.ast.get_type(from), self.ast.get_type(into)) {
					(
						Type::BuiltIn(
							BuiltInType::I32
							| BuiltInType::U8
							| BuiltInType::Bool
							| BuiltInType::UPtr,
						),
						Type::BuiltIn(BuiltInType::U8 | BuiltInType::I32 | BuiltInType::UPtr),
					) => {}
					(
						Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8 | BuiltInType::UPtr),
						Type::BuiltIn(BuiltInType::Bool),
					) => {}
					(Type::Pointer(_), Type::BuiltIn(BuiltInType::UPtr)) => {}
					(Type::Pointer(_), Type::Pointer(_)) => {}
					(Type::BuiltIn(BuiltInType::UPtr), Type::Pointer(_)) => {}
					(
						Type::BuiltIn(BuiltInType::IntLiteral),
						Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8 | BuiltInType::UPtr),
					) => {
						self.promote_int_literal_into(&mut child, into);
						return child;
					}
					_ => {
						self.error(parsed.span, location!())
							.message(format!(
								"Cannot cast {} into {}",
								self.format_type(from),
								self.format_type(into)
							))
							.build();
						return dummy_expr();
					}
				}
				child.cast_if_reference();

				Expression::new_spanned(into, ExpressionKind::Cast(child.into()), parsed.span)
			}
			parser::ExpressionKind::UnaryOperator(Operator::Reference, child) => {
				let child = self.check_expression(*child);
				if !child.value_type.reference {
					self.error(parsed.span, location!())
						.build_expected_reference();
				}
				let ty = self
					.ast
					.find_type_or_add(Type::Pointer(child.value_type.remove_reference()));
				Expression::new_spanned(
					ty,
					ExpressionKind::UnaryOperator(Operator::Reference, child.into()),
					parsed.span,
				)
			}
			parser::ExpressionKind::UnaryOperator(Operator::Dereference, child) => {
				let mut child = self.check_expression(*child);
				let Type::Pointer(inner) = self.ast.get_type(child.value_type) else {
					self.error(parsed.span, location!())
						.message("Dereference on non pointer")
						.description(format!(
							"Expected pointer, got {}",
							self.format_type(child.value_type.remove_reference())
						))
						.build();
					return dummy_expr();
				};
				child.cast_if_reference();
				Expression::new_spanned(
					inner.add_reference(),
					ExpressionKind::UnaryOperator(Operator::Dereference, child.into()),
					parsed.span,
				)
			}
			parser::ExpressionKind::Call(func_name, args) => {
				let mut call_args = args
					.into_iter()
					.map(|e| self.check_expression(e))
					.collect_vec();
				if let Some(calling_function) = self
					.ast
					.functions
					.iter()
					.find(|f| f.name == func_name)
					.or_else(|| {
						if func_name == self.function.name {
							Some(self.function)
						} else {
							None
						}
					}) {
					let func_arguments = calling_function.arguments.clone();

					if call_args.len() != func_arguments.len() {
						let kind = if call_args.len() < func_arguments.len() {
							"Not enough"
						} else {
							"Too many"
						};
						self.error(parsed.span, location!())
							.message(format!("{kind} arguments for function call"))
							.description(format!(
								"Expected {} arguments, got {}",
								func_arguments.len(),
								call_args.len()
							))
							.build();
						return dummy_expr();
					}

					for (arg, exp) in func_arguments.iter().zip(call_args.iter_mut()) {
						exp.cast_if_reference();
						let ty = self.promote_int_literal_into(exp, arg.ty);
						if ty != arg.ty {
							self.error(exp.span, location!())
								.message("Idk")
								.build_type_mismatch(ty, arg.ty);
						}
					}

					Expression::new_spanned(
						calling_function.return_type,
						ExpressionKind::Call(func_name, call_args),
						parsed.span,
					)
				} else {
					self.error(parsed.span, location!())
						.message(format!("Function {func_name} not found"))
						.build();
					dummy_expr()
				}
			}
			_ => todo!("{:?}", parsed),
		}
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

	fn format_type(&self, type_ref: TypeRef) -> String {
		type_ref.formatted(self.ast)
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
pub trait ProducesErrorWithAST: ProducesError {
	fn get_ast(&self) -> &AST;
}

impl ProducesErrorWithAST for TypeChecker {
	fn get_ast(&self) -> &AST {
		&self.ast
	}
}

impl ProducesErrorWithAST for FunctionTypeChecker<'_> {
	fn get_ast(&self) -> &AST {
		self.ast
	}
}
