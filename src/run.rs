use std::process::{Command, Output};

use crate::checker::TypeChecker;
use crate::compiler::Compiler;
use crate::graph::GraphGen;
use crate::lexer::*;
use crate::parser::Parser;

pub fn invoke_command(args: String) -> Output {
	return if cfg!(target_os = "windows") {
		Command::new("bash")
			.arg("-c")
			.arg(args)
			.output()
			.unwrap()
	}
	else {
		Command::new("sh")
			.arg("-c")
			.arg(args)
			.output()
			.unwrap()
	};
}

pub fn run<S: AsRef<std::path::Path>>(
	input: S,
	output_path: Option<String>,
	graph_file: Option<String>,
	build: bool,
) {
	let contents = match std::fs::read_to_string(input) {
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
			/* invoke nasm and linker */
			invoke_command(format!("nasm -f elf \"{0}\" -F dwarf -o \"{0}.o\"", output));
			invoke_command(format!("ld -m elf_i386 \"{0}.o\" -o \"{0}\"", output));
		}
	} else {
		print!("Compiler output:\n{}", asm_output);
	}
}
