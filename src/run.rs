use std::process::Command;

use crate::checker::TypeChecker;
use crate::compiler::Compiler;
use crate::graph::GraphGen;
use crate::lexer::*;
use crate::parser::Parser;

pub fn run<S: AsRef<std::path::Path>>(
	input: S,
	output_path: Option<String>,
	graph_file: Option<String>,
	build: bool,
) {
	let contents = std::fs::read_to_string(input).unwrap();

	let mut lexer = Lexer::new(contents.chars().peekable());
	let tokens: Vec<Token> = lexer.iter().collect();

	let mut parser = Parser::new(tokens.into_iter().peekable());
	parser.parse().unwrap();

	// println!("{:#?}", parser.functions);

	let mut checker = TypeChecker::new(&parser);
	checker.check().unwrap();
	let ast = checker.ast;

	// println!("{:#?}", ast.functions);

	if let Some(path) = graph_file {
		std::fs::write(path, GraphGen::generate_graph(&ast).unwrap()).unwrap();
	}

	let compiler = Compiler::new(ast);

	let asm_output = compiler.compile();

	if let Some(output) = output_path {
		std::fs::write(&output, include_str!("nasm.asm").to_string() + &asm_output).unwrap();
		if build {
			// use bash -c because im on windows and i want it to run on wsl :-)
			Command::new("bash")
				.arg("-c")
				.arg(format!("nasm -f elf \"{0}\" -F dwarf -o \"{0}.o\"", output))
				.output()
				.unwrap();

			Command::new("bash")
				.arg("-c")
				.arg(format!("ld -m elf_i386 \"{0}.o\" -o \"{0}\"", output))
				.output()
				.unwrap();
		}
	} else {
		print!("Compiler output:\n{}", asm_output);
	}
}
