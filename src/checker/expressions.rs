use std::collections::HashMap;

use itertools::Itertools;

use crate::{
	ast::{
		BuiltInType, Expression, ExpressionKind, HasAST, Type, TypeRef, BUILTIN_TYPE_BOOL,
		BUILTIN_TYPE_I32, BUILTIN_TYPE_INT_LITERAL, BUILTIN_TYPE_STR, BUILTIN_TYPE_VOID,
	},
	diagnostics::ProducesError,
	lexer::Operator,
	location,
	parser::{self},
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
						.build_unknown_struct_field(&field_name, struct_ty);
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
								.message("Function call argument type does not match")
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
						.message(format!("Function \"{func_name}\" not found"))
						.build();
					dummy_expr()
				}
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
					let mut expr = self.check_expression(parsed);
					expr.cast_if_reference();
					self.promote_int_literal_into(&mut expr, field.ty);
					if expr.value_type != field.ty {
						self.error(span, location!())
							.message("Field initializer type mismatch")
							.build_type_mismatch(expr.value_type, field.ty);
					}
					values.push((field_name, expr));
				}
				Expression::new_spanned(
					type_ref,
					ExpressionKind::StructLiteral(values),
					parsed.span,
				)
			}
			_ => todo!("{:?}", parsed),
		}
	}
}
