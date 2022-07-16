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
}

#[derive(Debug, Clone)]
pub enum OperatorToken {
	Assign,
	Add,
}

#[derive(Debug, Clone)]
pub enum TokenKind {
	Keyword(Keyword),
	Identifier(String),
	Number(i32),
	Operator(OperatorToken),
	Semicolon,
	LeftParen,
	RightParen,
	LeftBracket,
	RightBracket,
	TypeIndicator,
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

	pub fn get_token(&mut self) -> Option<Token> {
		let mut ch;
		while {
			ch = self.iterator.next()?;
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
				'=' => TokenKind::Operator(OperatorToken::Assign),
				'+' => TokenKind::Operator(OperatorToken::Add),
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
				ch => {
					let mut identifer: String = self
						.iterator
						.peeking_take_while(|c| c.is_alphanumeric())
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
