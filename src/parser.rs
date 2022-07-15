use crate::lexer::{Keyword, Token, TokenKind};

#[derive(Debug)]
pub enum ExpressionKind {
	NumberLiteral(i32),
	BoolLiteral(bool)
}

#[derive(Debug)]
pub struct Expression {
	kind: ExpressionKind,
	children: Vec<Expression>
}

impl Expression {
	pub fn new(kind: ExpressionKind, children: Vec<Expression>) -> Expression {
		Expression { kind, children }
	}
}

#[derive(Debug)]
pub enum StatementKind {
	Expression,
	Return
}

#[derive(Debug)]
pub struct Statement {
	kind: StatementKind,
	children: Vec<Expression>
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
	tokens: Vec<Token>,
	index: usize,
	pub functions: Vec<Function>
}

#[derive(Debug)]
pub enum ParserError<> {
	InvalidToken(Token),
}

impl Parser {
	pub fn new(tokens: Vec<Token>) -> Parser {
		Parser { tokens, index: 0, functions: Vec::new() }
	}

	fn peek(&self) -> &Token {
		&self.tokens[self.index]
	}

	fn next(&mut self) -> &Token {
		self.index += 1;
		&self.tokens[self.index - 1]
	}

	fn is_valid(&self) -> bool {
		self.index < self.tokens.len()
	}

	pub fn parse(&mut self) -> Result<(), ParserError> {
		while self.is_valid() {
			let token = self.next();
			match token.kind {
				TokenKind::Keyword(Keyword::Fn) => {
					let name;
					if let TokenKind::Identifier(str) = &self.next().kind {
						name = str.to_string();
					} else {
						return Err(ParserError::InvalidToken(token.clone()));
					}

					assert!(matches!(self.next().kind, TokenKind::LeftParen));
					// TODO: parse args
					assert!(matches!(self.next().kind, TokenKind::RightParen));

					// TODO: parse ret type

					let mut function = Function {
						name,
						statements: Vec::new(),
					};

					self.parse_block(&mut function.statements);

					self.functions.push(function);
				}
				_ => {
					return Err(ParserError::InvalidToken(token.clone()));
				}
			}
		}
		Ok(())
	}

	fn parse_block(&mut self, statements: &mut Vec<Statement>) {
		assert!(matches!(self.next().kind, TokenKind::LeftBracket));
		while !matches!(self.peek().kind, TokenKind::RightBracket) {
			statements.push(self.parse_statement());
			assert!(matches!(self.next().kind, TokenKind::Semicolon));
		}
		self.next(); // RightBracket
	}

	fn parse_statement(&mut self) -> Statement {
		let token = self.peek();
		match &token.kind {
			TokenKind::Keyword(Keyword::Return) => {
				self.next(); // Return
				Statement::new(StatementKind::Return, vec![self.parse_expression()])
			},
			_ => {
				Statement::new(StatementKind::Expression, vec![self.parse_expression()])
			}
		}
	}

	fn parse_expression(&mut self) -> Expression {
		let token = self.next();
		match &token.kind {
			TokenKind::Number(number) => {
				Expression::new(ExpressionKind::NumberLiteral(*number), vec![])
			},
			TokenKind::Keyword(value) if matches!(value, Keyword::True | Keyword::False) => {
				Expression::new(ExpressionKind::BoolLiteral(*value == Keyword::True), vec![])
			},
			kind => {
				todo!("Unimplemented expression {:?}", kind);
			}
		}
	}
}
