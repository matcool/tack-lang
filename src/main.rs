// SHUT UP!!!
#![allow(dead_code)]

use itertools::Itertools;

#[derive(Debug)]
enum Keyword {
	Fn,
	Let,
	Return,
	If,
	Else,
	While,
	True,
	False,
}

#[derive(Debug)]
enum OperatorToken {
	Assign,
	Add,
}

#[derive(Debug)]
enum TokenKind {
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

#[derive(Debug)]
struct Token {
	kind: TokenKind,
}

fn get_token<T: Iterator<Item = char>>(it: &mut std::iter::Peekable<T>) -> Option<Token> {
	let mut ch;
	while {
		ch = it.next()?;
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
				let mut number: String = it.peeking_take_while(|c| c.is_ascii_digit()).collect();
				// scary...
				number.insert(0, ch);
				// TODO: make this function return result
				TokenKind::Number(i32::from_str_radix(number.as_str(), 10).unwrap())
			},
			ch => {
				let mut identifer: String = it.peeking_take_while(|c| c.is_alphanumeric()).collect();
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

fn main() {
	let contents = std::fs::read_to_string("main.tack").unwrap();
	let mut it = contents.chars().peekable();
	loop {
		let token = get_token(&mut it);
		if token.is_none() {
			break;
		}
		println!("token is `{:?}`", token.unwrap());
	}
}
