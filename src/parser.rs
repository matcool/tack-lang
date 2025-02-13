use std::path::PathBuf;

use crate::{
	ast,
	diagnostics::ProducesError,
	lexer::{Attribute, Keyword, Operator, Token, TokenKind},
	span::Span,
};

#[derive(Debug, Clone)]
pub enum Type {
	Name(String),
	Pointer(Box<Type>),
	Array(Box<Type>, usize),
	Unknown, // used as a default value, shouldnt be used anywhere
}

#[derive(Debug, Clone)]
pub struct ParsedStruct {
	pub name: String,
	pub fields: Vec<Variable>,
}

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
	pub fn is_binary(&self) -> bool {
		!matches!(
			self,
			Operator::Not | Operator::Negate | Operator::Dereference | Operator::Reference
		)
	}
	fn is_right_associative(&self) -> bool {
		matches!(self, Operator::Assign)
	}
}

/// Represents a variable declaration, e.g. `x: i32`
#[derive(Debug, Clone)]
pub struct Variable {
	pub name: String,
	pub ty: Type,
}

#[derive(Debug)]
pub enum ExpressionKind {
	NumberLiteral(i64),
	BoolLiteral(bool),
	Declaration(Variable),
	Identifier(String),
	BinaryOperator(Operator, Box<Expression>, Box<Expression>),
	UnaryOperator(Operator, Box<Expression>),
	Call(String, Vec<Expression>),
	Cast(Type, Box<Expression>),
	StringLiteral(String),
	ArrayLiteral(Vec<Expression>),
	ArrayIndex(Box<Expression>, Box<Expression>),
	StructAccess(Box<Expression>, String),
}

#[derive(Debug)]
pub struct Expression {
	pub kind: ExpressionKind,
	pub span: Span,
}

impl Expression {
	pub fn new(kind: ExpressionKind) -> Expression {
		Expression {
			kind,
			span: Default::default(),
		}
	}
}

#[derive(Debug)]
pub enum StatementKind {
	Expression(Expression),
	Return(Option<Expression>),
	If(Scope, Expression, Option<Box<Statement>>),
	While(Scope, Expression),
	Block(Scope),
}

#[derive(Debug)]
pub struct Statement {
	pub kind: StatementKind,
	pub span: Span,
}

impl Statement {
	fn new(kind: StatementKind) -> Statement {
		Statement {
			kind,
			span: Default::default(),
		}
	}

	fn requires_semicolon(&self) -> bool {
		!matches!(
			&self.kind,
			StatementKind::If(_, _, _) | StatementKind::While(_, _) | StatementKind::Block(_)
		)
	}
}

#[derive(Debug)]
pub struct Scope {
	pub statements: Vec<Statement>,
}

impl Scope {
	fn new() -> Scope {
		Scope { statements: vec![] }
	}
}

#[derive(Debug)]
pub struct Function {
	pub name: String,
	pub arguments: Vec<Variable>,
	pub return_type: Type,
	pub scope: Scope,
	pub attributes: ast::FunctionAttributes,
}

impl Function {
	fn new(name: String) -> Function {
		Function {
			name,
			arguments: vec![],
			return_type: Type::Unknown,
			scope: Scope::new(),
			attributes: Default::default(),
		}
	}
}

pub struct Parser {
	tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
	pub functions: Vec<Function>,
	pub parsed_structs: Vec<ParsedStruct>,
	pub imported_files: Vec<String>,
	input_path: PathBuf,
	last_token_span: Span,
}

#[derive(Debug)]
pub enum ParserError {
	MissingToken,
}

macro_rules! error_at_token {
	($self:expr, $token:expr, $msg:expr) => {{
		let token = $token;
		$self.error(token.span, location!()).message($msg).build();
		unreachable!()
	}};
}

macro_rules! expect_token {
	($self:expr, $token:expr, $pattern:pat, $value:ident) => {{
		let token = $token;
		match token.kind {
			$pattern => Ok($value),
			_ => error_at_token!($self, token, "Unexpected token"),
		}
	}};
	($self:expr, $token:expr, $pattern:pat) => {{
		let token = $token;
		match token.kind {
			$pattern => Ok(token),
			_ => error_at_token!($self, token, "Unexpected token"),
		}
	}};
}

impl Parser {
	pub fn new(
		tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
		input_path: PathBuf,
	) -> Parser {
		Parser {
			tokens,
			functions: vec![],
			parsed_structs: vec![],
			imported_files: vec![],
			input_path,
			last_token_span: Default::default(),
		}
	}

	fn next(&mut self) -> Result<Token, ParserError> {
		match self.tokens.next() {
			Some(x) => {
				self.last_token_span = x.span;
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
					let mut function = self.parse_function_decl()?;
					let statements = self.parse_block()?;
					function.scope.statements = statements;
					self.functions.push(function);
				}
				TokenKind::Keyword(Keyword::Struct) => {
					let name = expect_token!(self, self.next()?, TokenKind::Identifier(x), x)?;

					let mut parsed_struct = ParsedStruct {
						name,
						fields: vec![],
					};

					expect_token!(self, self.next()?, TokenKind::LeftBrace)?;

					while !matches!(self.peek()?.kind, TokenKind::RightBrace) {
						parsed_struct.fields.push(self.parse_var_decl()?);
						expect_token!(self, self.next()?, TokenKind::Semicolon)?;
					}

					self.next()?; // RightBracket

					self.parsed_structs.push(parsed_struct);
				}
				TokenKind::Keyword(Keyword::Import) => {
					let file_path =
						expect_token!(self, self.next()?, TokenKind::StringLiteral(x), x)?;
					self.imported_files.push(file_path);
					expect_token!(self, self.next()?, TokenKind::Semicolon)?;
				}
				TokenKind::Attribute(Attribute::CExtern) => {
					expect_token!(self, self.next()?, TokenKind::Keyword(Keyword::Fn))?;
					let mut function = self.parse_function_decl()?;
					expect_token!(self, self.next()?, TokenKind::Semicolon)?;
					function.attributes.is_c_extern = true;
					self.functions.push(function);
				}
				_ => {
					error_at_token!(self, token, "Unexpected token at global scope");
				}
			}
		}
		Ok(())
	}

	/// Parse the very beginning of a function and return it, with an empty scope
	fn parse_function_decl(&mut self) -> Result<Function, ParserError> {
		let name = expect_token!(self, self.next()?, TokenKind::Identifier(x), x)?;

		let mut function = Function::new(name);

		expect_token!(self, self.next()?, TokenKind::LeftParen)?;
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
			TokenKind::LeftBrace => {
				function.return_type = Type::Name("void".to_string());
			}
			_ => {
				error_at_token!(self, self.next()?, "Expected function return type");
			}
		}

		Ok(function)
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
					error_at_token!(self, next, "Expected comma or end of list");
				}
			}
		}
		Ok(())
	}

	fn parse_block(&mut self) -> Result<Vec<Statement>, ParserError> {
		let mut result = Vec::new();
		expect_token!(self, self.next()?, TokenKind::LeftBrace)?;
		while !matches!(self.peek()?.kind, TokenKind::RightBrace) {
			let stmt = self.parse_statement()?;
			let semi = stmt.requires_semicolon();
			if semi {
				expect_token!(self, self.next()?, TokenKind::Semicolon)?;
			}
			result.push(stmt);
		}
		self.next()?; // RightBracket
		Ok(result)
	}

	fn parse_scope(&mut self) -> Result<Scope, ParserError> {
		let mut scope = Scope::new();
		scope.statements = self.parse_block()?;
		Ok(scope)
	}

	fn parse_var_decl(&mut self) -> Result<Variable, ParserError> {
		let name = expect_token!(self, self.next()?, TokenKind::Identifier(x), x)?;
		expect_token!(self, self.next()?, TokenKind::TypeIndicator)?;
		let ty = self.parse_type()?;
		Ok(Variable { name, ty })
	}

	fn parse_type(&mut self) -> Result<Type, ParserError> {
		let name = expect_token!(self, self.next()?, TokenKind::Identifier(x), x)?;
		let ty = Type::Name(name);
		match self.peek()?.kind {
			TokenKind::Operator(Operator::Multiply) => {
				self.next()?; // *
				  // TODO: multiple layers of pointers
				return Ok(Type::Pointer(Box::new(ty)));
			}
			TokenKind::LeftBracket => {
				let mut size = 0;
				self.next()?; // [
				if let TokenKind::Number(num) = self.peek()?.kind {
					self.next()?;
					size = num as usize;
				}
				self.next()?; // ]
				return Ok(Type::Array(Box::new(ty), size));
			}
			_ => {}
		}
		Ok(ty)
	}

	fn parse_statement(&mut self) -> Result<Statement, ParserError> {
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
				let condition = self.parse_expression()?;
				let scope = self.parse_scope()?;
				Statement::new(StatementKind::While(scope, condition))
			}
			TokenKind::Keyword(Keyword::If) => {
				self.next()?; // If
				let condition = self.parse_expression()?;
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
				self.parse_comma_list(|selfish: &mut Self| {
					args.push(selfish.parse_expression()?);
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
			TokenKind::Identifier(name) => Expression::new(ExpressionKind::Identifier(name)),
			TokenKind::Number(number) => Expression::new(ExpressionKind::NumberLiteral(number)),
			TokenKind::Keyword(value @ (Keyword::True | Keyword::False)) => {
				Expression::new(ExpressionKind::BoolLiteral(value == Keyword::True))
			}
			TokenKind::StringLiteral(content) => {
				Expression::new(ExpressionKind::StringLiteral(content))
			}
			TokenKind::LeftParen => {
				let exp = self.parse_expression()?;
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

	fn parse_expression(&mut self) -> Result<Expression, ParserError> {
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

	fn get_current_span(&mut self) -> Span {
		self.tokens.peek().map(|x| x.span).unwrap_or_default()
	}
}

impl ProducesError for Parser {
	fn file_path(&self) -> PathBuf {
		self.input_path.clone()
	}
	fn set_errored(&self) {
		std::process::exit(1);
	}
}
