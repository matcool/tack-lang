use itertools::{Itertools, PeekingNext};

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
	Fn,
	Let,
	Return,
	If,
	Else,
	While,
	True,
	False,
	Struct,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
	Assign,
	Add,
	Sub, // ☻
	Divide,
	Multiply,
	Equals,
	NotEquals,
	And,
	BitAnd,
	Dot,
	As,
	Mod,
	Or,
	BitOr,
	GreaterThan,
	LessThan,
	GreaterThanEq,
	LessThanEq,
	BitShiftLeft,
	BitShiftRight,
	// unary ops
	Not,
	Negate,
	Dereference,
	Reference,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
	Keyword(Keyword),
	Identifier(String),
	Number(i64),
	Operator(Operator),
	Semicolon,
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
	LeftBracket,
	RightBracket,
	TypeIndicator,
	Comma,
	StringLiteral(String),
}

#[derive(Debug, Clone)]
pub struct Span {
	pub line: i32,
	pub column: i32,
}

impl Default for Span {
	fn default() -> Self {
		Self { line: 0, column: 0 }
	}
}

#[derive(Debug, Clone)]
pub struct Token {
	pub kind: TokenKind,
	pub span: Span,
}

pub struct Lexer<I: Iterator<Item = char>> {
	iterator: std::iter::Peekable<I>,
	line: i32,
	column: i32,
}

pub struct LexerIterator<'a, I: Iterator<Item = char>> {
	lexer: &'a mut Lexer<I>,
}

impl<'a, I: Iterator<Item = char>> Iterator for LexerIterator<'a, I> {
	type Item = Token;

	fn next(&mut self) -> Option<Self::Item> {
		self.lexer.get_token()
	}
}

impl<I: Iterator<Item = char>> Lexer<I> {
	pub fn new(iterator: std::iter::Peekable<I>) -> Lexer<I> {
		Lexer {
			iterator,
			line: 1,
			column: 1,
		}
	}

	fn peek(&mut self) -> Option<char> {
		Some(*self.iterator.peek()?)
	}

	fn next(&mut self) -> Option<char> {
		let c = self.iterator.next();
		if let Some(c) = c {
			if c == '\n' {
				self.column = 1;
				self.line += 1;
			} else if c == '\t' {
				self.column += 4;
			} else {
				self.column += 1;
			}
		}
		c
	}

	pub fn get_token(&mut self) -> Option<Token> {
		let mut ch;
		while {
			ch = self.next()?;
			ch.is_whitespace()
		} {}
		let token = Token {
			span: Span {
				line: self.line,
				column: self.column - 1, // from the self.next() in the while loop before
			},
			kind: match ch {
				';' => TokenKind::Semicolon,
				'(' => TokenKind::LeftParen,
				')' => TokenKind::RightParen,
				'{' => TokenKind::LeftBrace,
				'}' => TokenKind::RightBrace,
				'[' => TokenKind::LeftBracket,
				']' => TokenKind::RightBracket,
				':' => TokenKind::TypeIndicator,
				',' => TokenKind::Comma,
				'.' => TokenKind::Operator(Operator::Dot),
				'+' => TokenKind::Operator(Operator::Add),
				'-' => TokenKind::Operator(Operator::Sub),
				'*' => TokenKind::Operator(Operator::Multiply),
				'%' => TokenKind::Operator(Operator::Mod),
				'>' => TokenKind::Operator(match self.peek()? {
					'>' => {
						self.next()?;
						Operator::BitShiftRight
					}
					'=' => {
						self.next()?;
						Operator::GreaterThanEq
					}
					_ => {
						self.next()?;
						Operator::GreaterThan
					}
				}),
				'<' => TokenKind::Operator(match self.peek()? {
					'<' => {
						self.next()?;
						Operator::BitShiftLeft
					}
					'=' => {
						self.next()?;
						Operator::LessThanEq
					}
					_ => {
						self.next()?;
						Operator::LessThan
					}
				}),
				'=' => {
					if self.peek()? == '=' {
						self.next()?;
						TokenKind::Operator(Operator::Equals)
					} else {
						TokenKind::Operator(Operator::Assign)
					}
				}
				'!' => {
					if self.peek()? == '=' {
						self.next()?;
						TokenKind::Operator(Operator::NotEquals)
					} else {
						TokenKind::Operator(Operator::Not)
					}
				}
				'&' => {
					if self.peek()? == '&' {
						self.next()?;
						TokenKind::Operator(Operator::And)
					} else {
						TokenKind::Operator(Operator::BitAnd)
					}
				}
				'|' => {
					if self.peek()? == '|' {
						self.next()?;
						TokenKind::Operator(Operator::Or)
					} else {
						TokenKind::Operator(Operator::BitOr)
					}
				}
				'/' => {
					if self.peek()? == '/' {
						self.take_while(|c| *c != '\n').for_each(drop);
						// use recursion to skip chars inside the match
						return self.get_token();
					} else {
						TokenKind::Operator(Operator::Divide)
					}
				}
				'0'..='9' => {
					if ch == '0' && self.peek().map(|c| c == 'x').unwrap_or(false) {
						self.next();
						let number: String =
							self.peeking_take_while(|c| c.is_ascii_hexdigit()).collect();

						TokenKind::Number(i64::from_str_radix(number.as_str(), 16).unwrap())
					} else {
						let mut number: String =
							self.peeking_take_while(|c| c.is_ascii_digit()).collect();
						// scary...
						number.insert(0, ch);
						// TODO: make this function return result
						TokenKind::Number(number.as_str().parse().unwrap())
					}
				}
				'"' => {
					let mut str = String::new();
					while self.peek()? != '"' {
						let c = self.next()?;
						if c == '\\' {
							match self.next()? {
								'n' => str.push('\n'),
								't' => str.push('\t'),
								'r' => str.push('\r'),
								'0' => str.push('\0'),
								'"' => str.push('"'),
								'\\' => str.push('\\'),
								// maybe add \x hex hex
								esc => panic!("invalid string escape {esc}"),
							}
						} else {
							str.push(c);
						}
					}
					self.next();
					TokenKind::StringLiteral(str)
				}
				ch => {
					let mut identifer: String = self
						.peeking_take_while(|c| c.is_alphanumeric() || c == &'_')
						.collect();
					// not very elegant :(
					identifer.insert(0, ch);
					match identifer.as_str() {
						"let" => TokenKind::Keyword(Keyword::Let),
						"fn" => TokenKind::Keyword(Keyword::Fn),
						"true" => TokenKind::Keyword(Keyword::True),
						"false" => TokenKind::Keyword(Keyword::False),
						"return" => TokenKind::Keyword(Keyword::Return),
						"if" => TokenKind::Keyword(Keyword::If),
						"else" => TokenKind::Keyword(Keyword::Else),
						"while" => TokenKind::Keyword(Keyword::While),
						"struct" => TokenKind::Keyword(Keyword::Struct),
						"as" => TokenKind::Operator(Operator::As),
						_ => TokenKind::Identifier(identifer),
					}
				}
			},
		};
		Some(token)
	}

	pub fn iter(&mut self) -> LexerIterator<I> {
		LexerIterator { lexer: self }
	}
}

impl<I: Iterator<Item = char>> Iterator for Lexer<I> {
	type Item = char;

	fn next(&mut self) -> Option<Self::Item> {
		self.next()
	}
}

impl<I: Iterator<Item = char>> PeekingNext for Lexer<I> {
	fn peeking_next<F: FnOnce(&Self::Item) -> bool>(&mut self, accept: F) -> Option<Self::Item> {
		if let Some(x) = self.peek() {
			if accept(&x) {
				return self.next();
			}
		}
		None
	}
}
