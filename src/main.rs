// SHUT UP!!!
#![allow(dead_code)]

mod checker;
mod compiler;
mod graph;
mod lexer;
mod parser;

use checker::TypeChecker;
use compiler::Compiler;
use graph::GraphGen;
use lexer::*;
use parser::Parser;

fn main() {
	let contents = std::fs::read_to_string("main.tack").unwrap();

	let mut lexer = Lexer::new(contents.chars().peekable());
	let tokens: Vec<Token> = lexer.iter().collect();

	let mut parser = Parser::new(tokens.into_iter().peekable());
	parser.parse().unwrap();

	let checker = TypeChecker::new(&parser);
	checker.check().unwrap();

	// println!("{:#?}", parser.functions);
	std::fs::write("graph.gv", GraphGen::generate_graph(&parser).unwrap()).unwrap();

	let compiler = Compiler::new(parser);

	println!("Compiler returned:\n{}", compiler.compile());
}
