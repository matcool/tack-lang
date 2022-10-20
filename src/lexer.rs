use itertools::Itertools;

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
	GreaterThan, // TODO:
	LessThan,
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
	Number(i32),
	Operator(Operator),
	Semicolon,
	LeftParen,
	RightParen,
	LeftBracket,
	RightBracket,
	TypeIndicator,
	Comma,
	StringLiteral(String),
}

#[derive(Debug, Clone)]
pub struct Token {
	pub kind: TokenKind,
}

pub struct Lexer<I: Iterator<Item = char>> {
	iterator: std::iter::Peekable<I>,
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
		Lexer { iterator }
	}

	fn peek(&mut self) -> Option<char> {
		Some(*self.iterator.peek()?)
	}

	fn next(&mut self) -> Option<char> {
		self.iterator.next()
	}

	pub fn get_token(&mut self) -> Option<Token> {
		let mut ch;
		while {
			ch = self.next()?;
			ch.is_whitespace()
		} {}
		let token = Token {
			kind: match ch {
				';' => TokenKind::Semicolon,
				'(' => TokenKind::LeftParen,
				')' => TokenKind::RightParen,
				'{' => TokenKind::LeftBracket,
				'}' => TokenKind::RightBracket,
				':' => TokenKind::TypeIndicator,
				',' => TokenKind::Comma,
				'.' => TokenKind::Operator(Operator::Dot),
				'+' => TokenKind::Operator(Operator::Add),
				'-' => TokenKind::Operator(Operator::Sub),
				'*' => TokenKind::Operator(Operator::Multiply),
				'%' => TokenKind::Operator(Operator::Mod),
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
						(&mut self.iterator)
							.take_while(|c| *c != '\n')
							.for_each(drop);
						// use recursion to skip chars inside the match
						return self.get_token();
					} else {
						TokenKind::Operator(Operator::Divide)
					}
				}
				'0'..='9' => {
					let mut number: String = self
						.iterator
						.peeking_take_while(|c| c.is_ascii_digit())
						.collect();
					// scary...
					number.insert(0, ch);
					// TODO: make this function return result
					TokenKind::Number(number.as_str().parse().unwrap())
				}
				'"' => {
					let str: String = self.iterator.peeking_take_while(|c| *c != '"').collect();
					self.next();
					TokenKind::StringLiteral(str)
				}
				ch => {
					let mut identifer: String = self
						.iterator
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
