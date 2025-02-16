use crate::{
	diagnostics::ProducesError,
	error_at_token,
	lexer::{Keyword, TokenKind},
};

use super::{Parser, ParserContext, ParserError, Statement, StatementKind};

impl Parser {
	pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
		let start = self.get_current_span();
		let token = self.peek()?;
		let mut stmt = match token.kind {
			TokenKind::Keyword(Keyword::Return) => {
				self.next()?; // Return
				let expr = if matches!(self.peek()?.kind, TokenKind::Semicolon) {
					None
				} else {
					Some(self.parse_expression()?)
				};
				Statement::new(StatementKind::Return(expr))
			}
			TokenKind::Keyword(Keyword::While) => {
				self.next()?; // While
				self.push_ctx(ParserContext {
					in_statement_condition: true,
				});
				let condition = self.parse_expression()?;
				self.pop_ctx();
				let scope = self.parse_scope()?;
				Statement::new(StatementKind::While(scope, condition))
			}
			TokenKind::Keyword(Keyword::If) => {
				self.next()?; // If
				self.push_ctx(ParserContext {
					in_statement_condition: true,
				});
				let condition = self.parse_expression()?;
				self.pop_ctx();
				let scope = self.parse_scope()?;
				let mut else_branch = None;
				if matches!(self.peek()?.kind, TokenKind::Keyword(Keyword::Else)) {
					self.next()?; // Else
					match self.peek()?.kind {
						TokenKind::LeftBrace | TokenKind::Keyword(Keyword::If) => {
							else_branch = Some(Box::new(self.parse_statement()?));
						}
						_ => {
							error_at_token!(self, self.next()?, "Expected if statement or block");
						}
					}
				}
				Statement::new(StatementKind::If(scope, condition, else_branch))
			}
			TokenKind::LeftBrace => Statement::new(StatementKind::Block(self.parse_scope()?)),
			_ => Statement::new(StatementKind::Expression(self.parse_expression()?)),
		};
		stmt.span = start.extended(self.last_token_span);
		Ok(stmt)
	}
}
