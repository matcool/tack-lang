// SHUT UP!!!
#![allow(dead_code)]

mod lexer;
use lexer::*;
mod parser;
use parser::Parser;

fn main() {
	let contents = std::fs::read_to_string("main.tack").unwrap();

	let mut lexer = Lexer::new(contents.chars().peekable());

	let tokens: Vec<Token> = lexer.iter().collect();

	let mut parser = Parser::new(tokens);

	parser.parse().unwrap();

	println!("{:#?}", parser.functions);
}
