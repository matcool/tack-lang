use crate::{
	diagnostics::ProducesError,
	error_at_token, expect_token,
	lexer::{Keyword, Operator, Token, TokenKind},
	span::Spannable,
};

use super::{Expression, ExpressionKind, Parser, ParserError};

impl Operator {
	const MAX_PRECEDENCE: i32 = 10;
	const CAST_PRECEDENCE: i32 = Self::MAX_PRECEDENCE;
	const PREFIX_PRECEDENCE: i32 = Self::MAX_PRECEDENCE + 1;
	const POSTFIX_PRECEDENCE: i32 = Self::MAX_PRECEDENCE + 2;
	fn precedence(&self) -> Option<i32> {
		Some(match self {
			Operator::Assign => 1,
			Operator::Or => 2,
			Operator::And => 3,
			Operator::BitOr => 4,
			Operator::BitAnd => 5,
			Operator::Equals | Operator::NotEquals => 6,
			Operator::GreaterThan
			| Operator::LessThan
			| Operator::GreaterThanEq
			| Operator::LessThanEq => 7,
			Operator::BitShiftLeft | Operator::BitShiftRight => 8,
			Operator::Add | Operator::Sub => 9,
			Operator::Multiply | Operator::Divide | Operator::Mod => 10,
			_ => None?,
		})
	}

	fn is_right_associative(&self) -> bool {
		matches!(self, Operator::Assign)
	}
}

impl Parser {
	fn get_expression_precedence(token: &Token) -> i32 {
		match token.kind {
			TokenKind::Operator(op) if op.precedence().is_some() => op.precedence().unwrap(),
			// Postfix
			TokenKind::LeftParen | TokenKind::LeftBracket | TokenKind::Operator(Operator::Dot) => {
				Operator::POSTFIX_PRECEDENCE
			}
			TokenKind::Operator(Operator::As) => Operator::CAST_PRECEDENCE,
			_ => 0,
		}
	}

	/// Parses either infix or postfix expressions.
	/// Precedence is defined in `Parser::get_expression_precedence`.
	fn parse_expression_infix(
		&mut self,
		token: Token,
		left: Expression,
	) -> Result<Expression, ParserError> {
		Ok(match token.kind {
			// All binary operators that define a precedence
			TokenKind::Operator(op) if op.is_binary() && op.precedence().is_some() => {
				let mut prec = op.precedence().unwrap();
				if op.is_right_associative() {
					prec -= 1;
				}
				let right = self.parse_expression_precedence(prec)?;
				Expression::new(ExpressionKind::BinaryOperator(
					op,
					left.into(),
					right.into(),
				))
			}
			// Postfix operators
			TokenKind::Operator(Operator::As) => {
				let ty = self.parse_type()?;
				Expression::new(ExpressionKind::Cast(ty, left.into()))
			}
			TokenKind::Operator(Operator::Dot) => {
				let name = expect_token!(self, self.next()?, TokenKind::Identifier(x), x)?;
				Expression::new(ExpressionKind::StructAccess(left.into(), name))
			}
			TokenKind::LeftParen => {
				let name = match left.kind {
					ExpressionKind::Identifier(name) => name,
					_ => unimplemented!("no dynamic calls yet"),
				};
				let mut args = Vec::new();
				self.parse_comma_list(TokenKind::RightParen, |this| {
					args.push(this.parse_expression()?);
					Ok(())
				})?;
				Expression::new(ExpressionKind::Call(name, args))
			}
			TokenKind::LeftBracket => {
				let index_exp = self.parse_expression()?;
				expect_token!(self, self.next()?, TokenKind::RightBracket)?;
				Expression::new(ExpressionKind::ArrayIndex(left.into(), index_exp.into()))
			}
			_ => {
				self.error(token.span, location!())
					.message("Unexpected token when parsing expression")
					.build();
				unreachable!();
			}
		})
	}

	fn parse_expression_prefix(&mut self, token: Token) -> Result<Expression, ParserError> {
		Ok(match token.kind {
			TokenKind::Identifier(name) => {
				if self.peek()?.kind == TokenKind::LeftBrace && !self.ctx().in_statement_condition {
					// Struct literal:
					// <ident> { <ident>: <expr>, ... }
					self.next()?; // {
					let mut values = Vec::new();
					self.parse_comma_list(TokenKind::RightBrace, |this| {
						let span = this.get_current_span();

						let name = expect_token!(this, this.next()?, TokenKind::Identifier(x), x)?;
						expect_token!(this, this.next()?, TokenKind::Colon)?;
						let expr = this.parse_expression()?;

						let span = span.extended(this.last_token_span);
						values.push((name, expr).spanned(span));
						Ok(())
					})?;
					Expression::new(ExpressionKind::StructLiteral(name, values))
				} else {
					Expression::new(ExpressionKind::Identifier(name))
				}
			}
			TokenKind::Number(number) => Expression::new(ExpressionKind::NumberLiteral(number)),
			TokenKind::Keyword(value @ (Keyword::True | Keyword::False)) => {
				Expression::new(ExpressionKind::BoolLiteral(value == Keyword::True))
			}
			TokenKind::StringLiteral(content) => {
				Expression::new(ExpressionKind::StringLiteral(content))
			}
			TokenKind::LeftParen => {
				self.push_ctx(Default::default());
				let exp = self.parse_expression()?;
				self.pop_ctx();
				expect_token!(self, self.next()?, TokenKind::RightParen)?;
				exp
			}
			TokenKind::Operator(
				op @ (Operator::Sub | Operator::Not | Operator::Multiply | Operator::BitAnd),
			) => {
				let child = self.parse_expression_precedence(Operator::PREFIX_PRECEDENCE)?;
				let op = match op {
					Operator::Sub => Operator::Negate,
					Operator::Multiply => Operator::Dereference,
					Operator::BitAnd => Operator::Reference,
					op => op,
				};
				Expression::new(ExpressionKind::UnaryOperator(op, child.into()))
			}
			TokenKind::LeftBracket => {
				let mut arr = Vec::new();
				while self.peek()?.kind != TokenKind::RightBracket {
					let exp = self.parse_expression()?;
					if self.peek()?.kind == TokenKind::Comma {
						self.next()?;
					} else {
						expect_token!(self, self.peek()?.clone(), TokenKind::RightBracket)?;
					}
					arr.push(exp);
				}
				self.next()?;
				Expression::new(ExpressionKind::ArrayLiteral(arr))
			}
			TokenKind::Keyword(Keyword::Let) => {
				let var = self.parse_var_decl()?;
				Expression::new(ExpressionKind::Declaration(var))
			}
			_ => {
				self.error(token.span, location!())
					.message("Unexpected token when parsing expression")
					.build();
				unreachable!();
			}
		})
	}

	pub fn parse_expression(&mut self) -> Result<Expression, ParserError> {
		self.parse_expression_precedence(0)
	}

	fn parse_expression_precedence(&mut self, precedence: i32) -> Result<Expression, ParserError> {
		let start = self.get_current_span();
		let token = self.next()?;
		let left = self.parse_expression_prefix(token)?;
		let mut result = left;
		result.span = start.extended(self.last_token_span);
		while precedence < self.get_next_infix_precedence()? {
			let token = self.next()?;
			result = self.parse_expression_infix(token, result)?;
			result.span = start.extended(self.last_token_span);
		}
		Ok(result)
	}

	fn get_next_infix_precedence(&mut self) -> Result<i32, ParserError> {
		let token = self.peek()?;
		Ok(Parser::get_expression_precedence(token))
	}
}
