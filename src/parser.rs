use crate::lexer::{Keyword, Operator, Token, TokenKind};
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
	pub name: String,
	pub reference: bool,
	pub pointer: bool,
}

impl Type {
	pub fn new<I: ToString>(name: I) -> Type {
		Type {
			name: name.to_string(),
			reference: false,
			pointer: false,
		}
	}
}

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.name)?;
		if self.pointer {
			write!(f, "*")?;
		}
		if self.reference {
			write!(f, "&")?;
		}
		Ok(())
	}
}

#[derive(Debug, Clone)]
pub struct Variable {
	pub name: String,
	pub ty: Type,
}

impl Operator {
	const MAX_PRECEDENCE: i32 = 3;
	fn precedence(&self) -> i32 {
		match self {
			Operator::Assign => 0,
			Operator::Equals | Operator::NotEquals => 2,
			Operator::Add | Operator::Sub => 2,
			Operator::Multiply | Operator::Divide => 3,
			_ => 9999,
		}
	}
	pub fn is_binary(&self) -> bool {
		match self {
			Operator::Not => false,
			Operator::Negate => false,
			Operator::Dereference => false,
			Operator::Reference => false,
			_ => true
		}
	}
}

#[derive(Debug)]
pub enum ExpressionKind {
	NumberLiteral(i32),
	BoolLiteral(bool),
	Declaration(Variable),
	Variable(String),
	Operator(Operator),
	Call(String),
	Cast,
}

#[derive(Debug)]
pub struct Expression {
	pub kind: ExpressionKind,
	pub children: Vec<Expression>,
	pub value_type: Type,
}

impl Expression {
	pub fn new(kind: ExpressionKind, children: Vec<Expression>) -> Expression {
		Expression {
			kind,
			children,
			value_type: Type::new("unknown"),
		}
	}
}

#[derive(Debug)]
pub enum StatementKind {
	Expression,
	Return,
	If(Rc<Scope>),
	While(Rc<Scope>),
	Block(Rc<Scope>),
}

#[derive(Debug)]
pub struct Statement {
	pub kind: StatementKind,
	pub children: Vec<Expression>,
	pub else_branch: Option<Box<Statement>>
}

impl Statement {
	fn new(kind: StatementKind, children: Vec<Expression>) -> Statement {
		Statement { kind, children, else_branch: None }
	}

	fn requires_semicolon(&self) -> bool {
		!matches!(&self.kind, StatementKind::If(_) | StatementKind::While(_))
	}
}

pub struct Scope {
	pub parent: Option<Rc<Scope>>,
	pub statements: Vec<RefCell<Statement>>,
	pub variables: RefCell<Vec<Variable>>,
}

impl std::fmt::Debug for Scope {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Scope")
			.field("parent", self.parent.as_ref().map_or(&"None", |_| &"Some(...)"))
			.field("statements", &self.statements)
			.field("variables", &self.variables)
			.finish()
	}
}

impl Scope {
	fn new(parent: Option<Rc<Scope>>) -> Scope {
		Scope {
			parent,
			statements: vec![],
			variables: vec![].into(),
		}
	}
}

#[derive(Debug)]
pub struct Function {
	pub name: String,
	pub arguments: Vec<Variable>,
	pub return_type: Type,
	pub scope: Rc<Scope>,
}

impl Function {
	fn new(name: String) -> Function {
		Function {
			name,
			arguments: vec![],
			return_type: Type::new("unknown"),
			scope: Rc::new(Scope::new(None)),
		}
	}
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
				// println!("Parser::next is {:?}", x);
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

					let mut function = Function::new(name);

					expect_token!(self.next()?, TokenKind::LeftParen)?;
					self.parse_comma_list(|selfish: &mut Self| {
						function.arguments.push(selfish.parse_var_decl()?);
						Ok(())
					})?;

					let next = self.peek()?;
					match next.kind {
						TokenKind::TypeIndicator => {
							self.next()?;
							function.return_type = self.parse_type()?;
						}
						TokenKind::LeftBracket => {
							function.return_type = Type::new("void");
						}
						_ => {
							return Err(ParserError::InvalidToken(self.next()?));
						}
					}

					self.parse_block(&mut Rc::get_mut(&mut function.scope).unwrap().statements)?;

					self.functions.push(function);
				}
				_ => {
					return Err(ParserError::InvalidToken(token));
				}
			}
		}
		Ok(())
	}

	fn parse_comma_list<C: FnMut(&mut Self) -> Result<(), ParserError>>(
		&mut self,
		mut callable: C,
	) -> Result<(), ParserError> {
		loop {
			if let TokenKind::RightParen = self.peek()?.kind {
				self.next()?;
				break;
			}

			callable(self)?;

			let next = self.next()?;
			match next.kind {
				TokenKind::Comma => {}
				TokenKind::RightParen => {
					break;
				}
				_ => {
					return Err(ParserError::InvalidToken(next));
				}
			}
		}
		Ok(())
	}

	fn parse_block(&mut self, statements: &mut Vec<RefCell<Statement>>) -> Result<(), ParserError> {
		expect_token!(self.next()?, TokenKind::LeftBracket)?;
		while !matches!(self.peek()?.kind, TokenKind::RightBracket) {
			let stmt = self.parse_statement()?;
			let semi = stmt.requires_semicolon();
			statements.push(stmt.into());
			if semi {
				expect_token!(self.next()?, TokenKind::Semicolon)?;
			}
		}
		self.next()?; // RightBracket
		Ok(())
	}

	fn parse_scope(&mut self, parent: Option<Rc<Scope>>) -> Result<Scope, ParserError> {
		let mut scope = Scope::new(parent);
		self.parse_block(&mut scope.statements)?;
		Ok(scope)
	}

	fn parse_var_decl(&mut self) -> Result<Variable, ParserError> {
		let name = expect_token!(self.next()?, TokenKind::Identifier(x), x)?;
		expect_token!(self.next()?, TokenKind::TypeIndicator)?;
		let ty = self.parse_type()?;
		Ok(Variable { name, ty })
	}

	fn parse_type(&mut self) -> Result<Type, ParserError> {
		let name = expect_token!(self.next()?, TokenKind::Identifier(x), x)?;
		let mut ty = Type::new(name);
		if self.peek()?.kind == TokenKind::Operator(Operator::Multiply) {
			self.next()?; // *
			ty.pointer = true;
		}
		Ok(ty)
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
			TokenKind::Keyword(Keyword::While) => {
				self.next()?; // While
				let condition = self.parse_expression()?;
				let scope = self.parse_scope(None)?;
				Ok(Statement::new(
					StatementKind::While(Rc::new(scope)),
					vec![condition],
				))
			}
			TokenKind::Keyword(Keyword::If) => {
				self.next()?; // If
				let condition = self.parse_expression()?;
				let scope = self.parse_scope(None)?;
				let mut stmt = Statement::new(
					StatementKind::If(Rc::new(scope)),
					vec![condition],
				);
				if matches!(self.peek()?.kind, TokenKind::Keyword(Keyword::Else)) {
					self.next()?; // Else
					match self.peek()?.kind {
						TokenKind::LeftBracket | TokenKind::Keyword(Keyword::If) => {
							stmt.else_branch = Some(Box::new(self.parse_statement()?));
						}
						_ => {
							return Err(ParserError::InvalidToken(self.next()?));
						}
					}
				}
				Ok(stmt)
			}
			TokenKind::LeftBracket => {
				Ok(Statement::new(StatementKind::Block(Rc::new(self.parse_scope(None)?)), vec![]))
			}
			_ => Ok(Statement::new(
				StatementKind::Expression,
				vec![self.parse_expression()?],
			)),
		}
	}

	fn parse_expression_primary(&mut self) -> Result<Expression, ParserError> {
		let token = self.next()?;
		match token.kind {
			TokenKind::Number(number) => Ok(Expression::new(
				ExpressionKind::NumberLiteral(number),
				vec![],
			)),
			TokenKind::Keyword(value @ (Keyword::True | Keyword::False)) => Ok(Expression::new(
				ExpressionKind::BoolLiteral(value == Keyword::True),
				vec![],
			)),
			TokenKind::Identifier(name) => {
				if let TokenKind::LeftParen = self.peek()?.kind {
					self.next()?; // LeftParen
					let mut exp = Expression::new(ExpressionKind::Call(name.clone()), vec![]);
					self.parse_comma_list(|selfish: &mut Self| {
						exp.children.push(selfish.parse_expression()?);
						Ok(())
					})?;
					Ok(exp)
				} else {
					Ok(Expression::new(ExpressionKind::Variable(name), vec![]))
				}
			}
			TokenKind::Keyword(Keyword::Let) => {
				let var = self.parse_var_decl()?;
				Ok(Expression::new(ExpressionKind::Declaration(var), vec![]))
			}
			TokenKind::LeftParen => {
				let exp = self.parse_expression()?;
				expect_token!(self.next()?, TokenKind::RightParen)?;
				Ok(exp)
			}
			TokenKind::Operator(op @ (Operator::Sub | Operator::Not | Operator::Multiply | Operator::BitAnd)) => {
				let child = self.parse_expression_primary()?;
				let op = match op {
					Operator::Sub => Operator::Negate,
					Operator::Multiply => Operator::Dereference,
					Operator::BitAnd => Operator::Reference,
					op => op
				};
				Ok(Expression::new(ExpressionKind::Operator(op), vec![child]))
			}
			kind => {
				todo!("expression {:?}", kind);
			}
		}
	}

	fn parse_expression_inner(&mut self, prec: i32) -> Result<Expression, ParserError> {
		if prec > Operator::MAX_PRECEDENCE {
			return self.parse_expression_primary();
		}
		let part = self.parse_expression_inner(prec + 1)?;
		let next = self.peek()?;
		match next.kind {
			TokenKind::Operator(operator) if operator.precedence() == prec => {
				self.next()?;
				Ok(Expression::new(
					ExpressionKind::Operator(operator),
					vec![part, self.parse_expression_inner(prec)?],
				))
			}
			_ => Ok(part),
		}
	}

	fn parse_expression(&mut self) -> Result<Expression, ParserError> {
		self.parse_expression_inner(0)
	}
}
