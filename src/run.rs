use std::path::PathBuf;
use std::process::{Command, Output};

use itertools::Itertools;
use path_slash::PathExt;

use crate::checker::TypeChecker;
use crate::compiler::Compiler;
// use crate::graph::GraphGen;
use crate::lexer::*;
use crate::parser::Parser;

pub fn invoke_command(args: String) -> Output {
	return if cfg!(windows) {
		Command::new("bash").arg("-c").arg(args).output().unwrap()
	} else {
		Command::new("sh").arg("-c").arg(args).output().unwrap()
	};
}

pub fn run<S: AsRef<std::path::Path> + Into<PathBuf> + Clone>(
	input_path: S,
	output_path: Option<String>,
	graph_file: Option<String>,
	build: bool,
) {
	let contents = match std::fs::read_to_string(input_path.clone()) {
		Ok(value) => value,
		Err(value) => {
			println!("Error: {value}");
			std::process::exit(1)
		}
	};

	let mut lexer = Lexer::new(contents.chars().peekable());
	let tokens: Vec<Token> = lexer.iter().collect();

	let mut parser = Parser::new(tokens.into_iter().peekable());
	parser.parse().unwrap();

	let asts = TypeChecker::new(input_path.into()).check(parser).unwrap();
	let ast = &asts[0];

	crate::dump::dump(ast);

	let compiled = Compiler::new(ast).compile();
	if let Some(output) = output_path {
		std::fs::write(&output, compiled).unwrap();
	} else {
		println!("{compiled}");
	}
}
