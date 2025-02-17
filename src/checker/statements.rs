use crate::{
	ast::{Statement, StatementKind, BUILTIN_TYPE_BOOL, BUILTIN_TYPE_VOID},
	diagnostics::ProducesError,
	location,
	parser::{self},
};

use super::FunctionTypeChecker;

impl FunctionTypeChecker<'_> {
	pub fn check_statement(&mut self, parsed: parser::Statement) -> Statement {
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
					let mut expr = self.check_expression(expr).into_cast_ref();
					let ty = self.promote_int_literal_into(&mut expr, self.function.return_type);
					if ty != self.function.return_type {
						self.error(expr.span, location!())
							.message("Expression does not match return type")
							.build_type_mismatch(expr.ty, self.function.return_type);
					}
					Statement::new(StatementKind::Return(Some(expr)), parsed.span)
				}
			}
			parser::StatementKind::If(parsed_scope, condition, else_stmt) => {
				let condition = self.check_expression(condition).into_cast_ref();
				if condition.ty != BUILTIN_TYPE_BOOL {
					self.error(condition.span, location!())
						.message("Condition must be boolean")
						.build_type_mismatch(condition.ty, BUILTIN_TYPE_BOOL);
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
				let condition = self.check_expression(condition).into_cast_ref();
				if condition.ty != BUILTIN_TYPE_BOOL {
					self.error(condition.span, location!())
						.message("Condition must be boolean")
						.build_type_mismatch(condition.ty, BUILTIN_TYPE_BOOL);
				}
				let scope = self.check_scope(parsed_scope);
				Statement::new(StatementKind::While(scope, condition), parsed.span)
			}
		}
	}
}
