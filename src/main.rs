// SHUT UP!!!
#![allow(dead_code)]

mod lexer;
use lexer::*;
mod parser;
use parser::Parser;
mod checker;
use checker::TypeChecker;
mod compiler;
use compiler::Compiler;

fn main() {
	let contents = std::fs::read_to_string("main.tack").unwrap();

	let mut lexer = Lexer::new(contents.chars().peekable());
	let tokens: Vec<Token> = lexer.iter().collect();

	let mut parser = Parser::new(tokens.into_iter().peekable());
	parser.parse().unwrap();

	println!("{:#?}", parser.functions);

	let mut checker = TypeChecker::new(&mut parser);
	checker.check().unwrap();

	drop(checker);

	let compiler = Compiler::new(parser);

	println!("Compiler returned:\n{}", compiler.compile());
}
