use crate::lexer::{Keyword, Token, TokenKind};

#[derive(Debug)]
pub enum ExpressionKind {
	NumberLiteral(i32),
	BoolLiteral(bool),
}

#[derive(Debug)]
pub struct Expression {
	kind: ExpressionKind,
	children: Vec<Expression>,
}

impl Expression {
	pub fn new(kind: ExpressionKind, children: Vec<Expression>) -> Expression {
		Expression { kind, children }
	}
}

#[derive(Debug)]
pub enum StatementKind {
	Expression,
	Return,
}

#[derive(Debug)]
pub struct Statement {
	kind: StatementKind,
	children: Vec<Expression>,
}

impl Statement {
	fn new(kind: StatementKind, children: Vec<Expression>) -> Statement {
		Statement { kind, children }
	}
}

#[derive(Debug)]
pub struct Function {
	name: String,
	statements: Vec<Statement>,
}

pub struct Parser {
	tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
	// index: usize,
	pub functions: Vec<Function>,
}

#[derive(Debug)]
pub enum ParserError {
	InvalidToken(Token),
	MissingToken, // for when the iterator reaches the end, def need a better name
}

macro_rules! expect_token {
	($token:expr, $pattern:pat, $value:ident) => {{
		let token = $token;
		match token.kind {
			$pattern => Ok($value),
			_ => Err(ParserError::InvalidToken(token)),
		}
	}};
	($token:expr, $pattern:pat) => {{
		let token = $token;
		match token.kind {
			$pattern => Ok(token),
			_ => Err(ParserError::InvalidToken(token)),
		}
	}};
}

impl Parser {
	pub fn new(tokens: std::iter::Peekable<std::vec::IntoIter<Token>>) -> Parser {
		Parser {
			tokens,
			functions: vec![],
		}
	}

	fn next(&mut self) -> Result<Token, ParserError> {
		match self.tokens.next() {
			Some(x) => {
				println!("Parser::next is {:?}", x);
				Ok(x)
			}
			None => Err(ParserError::MissingToken),
		}
	}

	fn peek(&mut self) -> Result<&Token, ParserError> {
		match self.tokens.peek() {
			Some(x) => Ok(x),
			None => Err(ParserError::MissingToken),
		}
	}

	pub fn parse(&mut self) -> Result<(), ParserError> {
		while let Ok(token) = self.next() {
			match token.kind {
				TokenKind::Keyword(Keyword::Fn) => {
					let name = expect_token!(self.next()?, TokenKind::Identifier(x), x)?;

					expect_token!(self.next()?, TokenKind::LeftParen)?;
					// TODO: parse args
					expect_token!(self.next()?, TokenKind::RightParen)?;

					// TODO: parse ret type

					let mut function = Function {
						name,
						statements: vec![],
					};

					self.parse_block(&mut function.statements)?;

					self.functions.push(function);
				}
				_ => {
					return Err(ParserError::InvalidToken(token));
				}
			}
		}
		Ok(())
	}

	fn parse_block(&mut self, statements: &mut Vec<Statement>) -> Result<(), ParserError> {
		expect_token!(self.next()?, TokenKind::LeftBracket)?;
		while !matches!(self.peek()?.kind, TokenKind::RightBracket) {
			statements.push(self.parse_statement()?);
			expect_token!(self.next()?, TokenKind::Semicolon)?;
		}
		self.next()?; // RightBracket
		Ok(())
	}

	fn parse_statement(&mut self) -> Result<Statement, ParserError> {
		let token = self.peek()?;
		match token.kind {
			TokenKind::Keyword(Keyword::Return) => {
				self.next()?; // Return
				Ok(Statement::new(
					StatementKind::Return,
					vec![self.parse_expression()?],
				))
			}
			_ => Ok(Statement::new(
				StatementKind::Expression,
				vec![self.parse_expression()?],
			)),
		}
	}

	fn parse_expression(&mut self) -> Result<Expression, ParserError> {
		let token = self.next()?;
		match token.kind {
			TokenKind::Number(number) => Ok(Expression::new(
				ExpressionKind::NumberLiteral(number),
				vec![],
			)),
			TokenKind::Keyword(value @ (Keyword::True | Keyword::False))  => Ok(
				Expression::new(ExpressionKind::BoolLiteral(value == Keyword::True), vec![]),
			),
			kind => {
				todo!("Unimplemented expression {:?}", kind);
			}
		}
	}
}
