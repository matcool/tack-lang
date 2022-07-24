// SHUT UP!!!
#![allow(dead_code)]

mod checker;
mod compiler;
mod graph;
mod lexer;
mod parser;

use std::process::Command;

use checker::TypeChecker;
use compiler::Compiler;
use graph::GraphGen;
use lexer::*;
use parser::Parser;

fn print_help_and_exit() -> ! {
	println!(
		"tack compiler. very silly language

Usage: tack input [opts]

  input - input file to compile
  opts:
    -o output - output asm file
    -b --build - build using nasm and ld
"
	);
	std::process::exit(1);
}

fn main() {
	let input = match std::env::args().skip(1).next() {
		Some(value) => value,
		_ => print_help_and_exit(),
	};
	let mut output = None;
	let mut build = false;
	let mut iter = std::env::args().skip(2);
	loop {
		if let Some(arg) = iter.next() {
			if arg == "-o" {
				output = iter.next();
			} else if arg == "-b" || arg == "--build" {
				build = true;
			} else {
				println!("Unknown option \"{}\"", arg);
				print_help_and_exit();
			}
		} else {
			break;
		}
	}

	let contents = std::fs::read_to_string(input).unwrap();

	let mut lexer = Lexer::new(contents.chars().peekable());
	let tokens: Vec<Token> = lexer.iter().collect();

	let mut parser = Parser::new(tokens.into_iter().peekable());
	parser.parse().unwrap();

	let checker = TypeChecker::new(&parser);
	checker.check().unwrap();

	// println!("{:#?}", parser.functions);
	std::fs::write("graph.gv", GraphGen::generate_graph(&parser).unwrap()).unwrap();

	let compiler = Compiler::new(parser);

	let asm_output = compiler.compile();

	if let Some(output_path) = output {
		std::fs::write(
			&output_path,
			include_str!("nasm.asm").to_string() + &asm_output,
		).unwrap();
		if build {
			Command::new("bash")
				.arg("-c")
				.arg(format!("nasm -f elf \"{0}\" -o \"{0}.o\"", output_path))
				.output()
				.unwrap();

			Command::new("bash")
				.arg("-c")
				.arg(format!("ld -m elf_i386 \"{0}.o\" -o \"{0}\"", output_path))
				.output()
				.unwrap();
		}
	} else {
		print!("Compiler output:\n{}", asm_output);
	}
}
