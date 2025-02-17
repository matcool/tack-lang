use std::{cell::RefCell, path::PathBuf};

use crate::{
	ast,
	diagnostics::ProducesError,
	lexer::{Attribute, Keyword, Operator, Token, TokenKind},
	span::Span,
};

mod expressions;
mod statements;

// dont want to have to write parser::ast::...
include!("./ast.rs");

pub struct Parser {
	tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
	pub functions: Vec<Function>,
	pub parsed_structs: Vec<ParsedStruct>,
	pub imported_files: Vec<String>,
	input_path: PathBuf,
	last_token_span: Span,
	context_stack: RefCell<Vec<ParserContext>>,
}

#[derive(Default, Copy, Clone)]
struct ParserContext {
	/// This is used to signal the struct literal parser, since it might be ambiguous otherwise
	in_statement_condition: bool,
}

#[derive(Debug)]
pub enum ParserError {
	MissingToken,
}

#[macro_export]
macro_rules! error_at_token {
	($self:expr, $token:expr, $msg:expr) => {{
		let token = $token;
		$self.error(token.span, location!()).message($msg).build();
		unreachable!()
	}};
}

#[macro_export]
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
			context_stack: Default::default(),
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
					let mut function = self.parse_function_decl(false)?;
					let statements = self.parse_block()?;
					function.scope.statements = statements;
					self.functions.push(function);
				}
				TokenKind::Keyword(Keyword::Struct) => {
					let name = expect_token!(self, self.next()?, TokenKind::Identifier(x), x)?;

					let mut parsed_struct = ParsedStruct {
						name,
						fields: vec![],
						functions: vec![],
					};

					expect_token!(self, self.next()?, TokenKind::LeftBrace)?;

					while !matches!(self.peek()?.kind, TokenKind::RightBrace) {
						match self.peek()?.kind {
							TokenKind::Keyword(Keyword::Fn) => {
								self.next()?;
								let mut function = self.parse_function_decl(true)?;
								function.scope.statements = self.parse_block()?;
								parsed_struct.functions.push(function);
							}
							_ => {
								parsed_struct.fields.push(self.parse_var_decl()?);
								expect_token!(self, self.next()?, TokenKind::Semicolon)?;
							}
						}
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
					let mut function = self.parse_function_decl(false)?;
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
	fn parse_function_decl(&mut self, inside_struct: bool) -> Result<Function, ParserError> {
		let name = expect_token!(self, self.next()?, TokenKind::Identifier(x), x)?;

		let mut function = Function::new(name);

		expect_token!(self, self.next()?, TokenKind::LeftParen)?;
		self.parse_comma_list(TokenKind::RightParen, |this| {
			if inside_struct && this.peek()?.kind == TokenKind::Keyword(Keyword::SmallSelf) {
				let span = this.next()?.span;
				// TODO: this is kinda nasty
				function.arguments.push(Variable {
					name: "self".into(),
					span,
					ty: Type::Unknown,
				});
			} else {
				function.arguments.push(this.parse_var_decl()?);
			}
			Ok(())
		})?;

		let next = self.peek()?;
		match next.kind {
			TokenKind::Colon => {
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
		terminator: TokenKind,
		mut callable: C,
	) -> Result<(), ParserError> {
		loop {
			if self.peek()?.kind == terminator {
				self.next()?;
				break;
			}

			callable(self)?;

			let next = self.next()?;
			match next.kind {
				TokenKind::Comma => {}
				ref k => {
					if k == &terminator {
						break;
					} else {
						error_at_token!(self, next, "Expected comma or end of list");
					}
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
		let start = self.get_current_span();
		let name = expect_token!(self, self.next()?, TokenKind::Identifier(x), x)?;
		expect_token!(self, self.next()?, TokenKind::Colon)?;
		let ty = self.parse_type()?;
		let span = start.extended(self.last_token_span);
		Ok(Variable { name, ty, span })
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

	fn get_current_span(&mut self) -> Span {
		self.tokens.peek().map(|x| x.span).unwrap_or_default()
	}

	fn ctx(&self) -> ParserContext {
		self.context_stack
			.try_borrow()
			.ok()
			.and_then(|s| s.last().copied())
			.unwrap_or_default()
	}

	fn push_ctx(&self, ctx: ParserContext) {
		self.context_stack.borrow_mut().push(ctx);
	}

	fn pop_ctx(&self) {
		self.context_stack.borrow_mut().pop();
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
