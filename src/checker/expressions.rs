use std::collections::HashMap;

use itertools::Itertools;

use crate::{
	ast::{
		BuiltInType, Expression, ExpressionKind, HasAST, Type, TypeRef, Variable,
		BUILTIN_TYPE_BOOL, BUILTIN_TYPE_I32, BUILTIN_TYPE_INT_LITERAL, BUILTIN_TYPE_STR,
		BUILTIN_TYPE_VOID,
	},
	diagnostics::ProducesError,
	lexer::Operator,
	location,
	parser::{self},
	span::Span,
};

use super::{dummy_expr, FunctionTypeChecker};

impl FunctionTypeChecker<'_> {
	pub fn check_expression(&mut self, parsed: parser::Expression) -> Expression {
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
				let mut right = self.check_expression(*right).into_cast_ref();
				let left = self.check_expression(*left);

				let left_ty = left.ty;
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
				let mut left = self.check_expression(*left).into_cast_ref();
				let mut right = self.check_expression(*right).into_cast_ref();

				// Promote rhs into lhs if possible
				let right_ty = self.promote_int_literal_into(&mut right, left.ty);
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
							.build_type_mismatch(right.ty, BUILTIN_TYPE_I32);
					}
					return Expression::new_spanned(
						left_ty,
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

				let ty = match op {
					Operator::Equals
					| Operator::NotEquals
					| Operator::GreaterThan
					| Operator::GreaterThanEq
					| Operator::LessThan
					| Operator::LessThanEq => BUILTIN_TYPE_BOOL,
					_ => left_ty,
				};
				Expression::new_spanned(
					ty,
					ExpressionKind::BinaryOperator(op, left.into(), right.into()),
					parsed.span,
				)
			}
			parser::ExpressionKind::UnaryOperator(Operator::Negate, child) => {
				let child = self.check_expression(*child).into_cast_ref();
				if child.ty != BUILTIN_TYPE_INT_LITERAL && !self.ast.is_integer(child.ty) {
					self.error(parsed.span, location!())
						.message("Negation must be done on integers")
						.build_type_mismatch(child.ty, BUILTIN_TYPE_INT_LITERAL);
				}
				Expression::new_spanned(
					child.ty,
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
				match self.ast.get_type(struct_expr.ty) {
					Type::Struct(_) => {
						struct_ty = Some(struct_expr.ty);
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
				if self.ast.is_pointer(struct_expr.ty) {
					let casted = struct_expr.into_cast_ref();
					struct_expr = Expression::new(
						struct_ty.add_reference(),
						ExpressionKind::UnaryOperator(Operator::Dereference, casted.into()),
					);
				}
				let Type::Struct(struct_ty) = self.ast.get_type(struct_ty) else {
					unreachable!()
				};
				if let Some(field) = struct_ty.fields.iter().find(|f| f.name == field_name) {
					let ty = if struct_expr.ty.reference {
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
						.build_unknown_struct_field(&field_name, struct_ty);
					dummy_expr()
				}
			}
			parser::ExpressionKind::ArrayLiteral(values) => {
				let mut values = values
					.into_iter()
					.map(|e| self.check_expression(e).into_cast_ref())
					.collect_vec();

				let mut inner_type = TypeRef::unknown();
				for child in &mut values {
					if inner_type.is_unknown() {
						inner_type = child.ty;
					} else {
						self.promote_int_literal_into(child, inner_type);
						if inner_type != child.ty {
							self.error(parsed.span, location!())
								.message(format!(
									"Array expected {}, got {}",
									self.format_type(inner_type),
									self.format_type(child.ty)
								))
								.build_type_mismatch(child.ty, inner_type);
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
				let mut index_expr = self.check_expression(*index_expr).into_cast_ref();
				let index_type = self.promote_int_literal_into(&mut index_expr, BUILTIN_TYPE_I32);
				if !self.ast.is_integer(index_type) {
					self.error(parsed.span, location!())
						.message("Arrays must be indexed with integers")
						.build_type_mismatch(index_type, BUILTIN_TYPE_INT_LITERAL);
				}

				let arr_expr = self.check_expression(*arr_expr).into_cast_ref();
				let arr_type = arr_expr.ty;

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
				let mut child = self.check_expression(*child).into_cast_ref();
				let from = child.ty;

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

				Expression::new_spanned(into, ExpressionKind::Cast(child.into()), parsed.span)
			}
			parser::ExpressionKind::UnaryOperator(Operator::Reference, child) => {
				let child = self.check_expression(*child);
				if !child.ty.reference {
					self.error(parsed.span, location!())
						.build_expected_reference();
				}
				let ty = self
					.ast
					.find_type_or_add(Type::Pointer(child.ty.remove_reference()));
				Expression::new_spanned(
					ty,
					ExpressionKind::UnaryOperator(Operator::Reference, child.into()),
					parsed.span,
				)
			}
			parser::ExpressionKind::UnaryOperator(Operator::Dereference, child) => {
				let child = self.check_expression(*child).into_cast_ref();
				let Type::Pointer(inner) = self.ast.get_type(child.ty) else {
					self.error(parsed.span, location!())
						.message("Dereference on non pointer")
						.description(format!(
							"Expected pointer, got {}",
							self.format_type(child.ty.remove_reference())
						))
						.build();
					return dummy_expr();
				};
				Expression::new_spanned(
					inner.add_reference(),
					ExpressionKind::UnaryOperator(Operator::Dereference, child.into()),
					parsed.span,
				)
			}
			parser::ExpressionKind::Call(func_name, args) => {
				let call_args = args
					.into_iter()
					.map(|e| self.check_expression(e).into_cast_ref())
					.collect_vec();
				let Some(calling_function) = self
					.ast
					.functions
					.iter()
					.find(|f| f.name == func_name)
					.or_else(|| (func_name == self.function.name).then_some(self.function))
				else {
					self.error(parsed.span, location!())
						.message(format!("Function \"{func_name}\" not found"))
						.build();
					return dummy_expr();
				};
				let call_args =
					self.check_call_args(parsed.span, call_args, &calling_function.arguments);

				Expression::new_spanned(
					calling_function.return_type,
					ExpressionKind::Call(func_name, call_args),
					parsed.span,
				)
			}
			parser::ExpressionKind::StructLiteral(struct_name, initializers) => {
				let type_ref = self.ast.find_type_by_name(&struct_name);
				let Some(Type::Struct(struct_type)) = type_ref.map(|ty| self.ast.get_type(ty))
				else {
					self.error(parsed.span, location!())
						.message(format!("\"{struct_name}\" is not a known struct type"))
						.build();
					return dummy_expr();
				};
				let type_ref = type_ref.unwrap();
				// lovely
				let struct_type = struct_type.clone();

				let mut seen = HashMap::new();
				let mut values = Vec::new();
				for initializer in initializers {
					let span = initializer.span;
					let (field_name, parsed) = initializer.value;
					let Some(field) = struct_type.fields.iter().find(|f| f.name == field_name)
					else {
						self.error(span, location!())
							.build_unknown_struct_field(&field_name, &struct_type);
						continue;
					};
					if let Some(previous_span) = seen.get(&field_name) {
						self.error(span, location!())
							.message("Duplicate field in struct literal")
							.extra(*previous_span, "Previously declared here")
							.build();
						continue;
					}
					seen.insert(field_name.clone(), span);
					let mut expr = self.check_expression(parsed).into_cast_ref();
					self.promote_int_literal_into(&mut expr, field.ty);
					if expr.ty != field.ty {
						self.error(span, location!())
							.message("Field initializer type mismatch")
							.build_type_mismatch(expr.ty, field.ty);
					}
					values.push((field_name, expr));
				}
				Expression::new_spanned(
					type_ref,
					ExpressionKind::StructLiteral(values),
					parsed.span,
				)
			}
			parser::ExpressionKind::MethodCall(struct_expr, name, args) => {
				let mut struct_expr = self.check_expression(*struct_expr);
				let struct_type_ref;
				if let Type::Pointer(inner) = self.ast.get_type(struct_expr.ty) {
					struct_expr = struct_expr.into_cast_ref();
					struct_type_ref = *inner;
				} else {
					// TODO: extend lifetime if struct is a temporary or something
					if !struct_expr.ty.reference {
						self.error(parsed.span, location!())
							.message("cant do temporaries yet")
							.build();
						return dummy_expr();
					}
					struct_type_ref = struct_expr.ty;
					let span = struct_expr.span;
					// the `self` arg in methods is just a pointer, so synthesize one
					struct_expr = Expression::new_spanned(
						self.ast.find_type_or_add(Type::Pointer(struct_expr.ty)),
						ExpressionKind::UnaryOperator(Operator::Reference, struct_expr.into()),
						span,
					);
				}

				let mut args: Vec<Expression> = args
					.into_iter()
					.map(|e| self.check_expression(e).into_cast_ref())
					.collect_vec();

				let Type::Struct(_) = self.ast.get_type(struct_type_ref) else {
					self.error(struct_expr.span, location!())
						.message("Expected struct")
						.build();
					return dummy_expr();
				};
				let Some(function) = self.ast.functions.iter().find(|f| f.name == name) else {
					self.error(parsed.span, location!())
						.message("Unknown method {}")
						.build();
					return dummy_expr();
				};

				args.insert(0, struct_expr);
				let args = self.check_call_args(parsed.span, args, &function.arguments);

				Expression::new_spanned(
					function.return_type,
					ExpressionKind::Call(name, args),
					parsed.span,
				)
			}
			_ => todo!("{:?}", parsed),
		}
	}

	fn check_call_args(
		&self,
		span: Span,
		unchecked_args: Vec<Expression>,
		function_args: &[Variable],
	) -> Vec<Expression> {
		if unchecked_args.len() != function_args.len() {
			let kind = if unchecked_args.len() < function_args.len() {
				"Not enough"
			} else {
				"Too many"
			};
			self.error(span, location!())
				.message(format!("{kind} arguments for function call"))
				.description(format!(
					"Expected {} arguments, got {}",
					function_args.len(),
					unchecked_args.len()
				))
				.build();
			return vec![];
		}

		unchecked_args
			.into_iter()
			.zip(function_args.iter())
			.map(|(mut call_arg, function_arg)| {
				let ty = self.promote_int_literal_into(&mut call_arg, function_arg.ty);
				if ty != function_arg.ty {
					self.error(call_arg.span, location!())
						.message("Function call argument type does not match")
						.build_type_mismatch(ty, function_arg.ty);
				}
				call_arg
			})
			.collect()
	}
}
