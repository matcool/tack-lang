// SHUT UP!!!
#![allow(dead_code)]

mod lexer;
mod parser;
mod checker;
mod compiler;

use lexer::*;
use parser::Parser;
use checker::TypeChecker;
use compiler::Compiler;

fn main() {
	let contents = std::fs::read_to_string("main.tack").unwrap();

	let mut lexer = Lexer::new(contents.chars().peekable());
	let tokens: Vec<Token> = lexer.iter().collect();

	let mut parser = Parser::new(tokens.into_iter().peekable());
	parser.parse().unwrap();

	let checker = TypeChecker::new(&parser);
	checker.check().unwrap();
	
	println!("{:#?}", parser.functions);
	
	let compiler = Compiler::new(parser);

	println!("Compiler returned:\n{}", compiler.compile());
}
