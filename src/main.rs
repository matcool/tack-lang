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
#[cfg(test)]
use path_slash::PathBufExt;

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
	let input = match std::env::args().nth(1) {
		Some(value) => value,
		_ => print_help_and_exit(),
	};
	let mut output = None;
	let mut build = false;
	let mut iter = std::env::args().skip(2);
	while let Some(arg) = iter.next() {
		if arg == "-o" {
			output = iter.next();
		} else if arg == "-b" || arg == "--build" {
			build = true;
		} else {
			println!("Unknown option \"{}\"", arg);
			print_help_and_exit();
		}
	}

	run(input, output, Some("graph.gv".into()), build);
}

fn run<S: AsRef<std::path::Path>>(input: S, output_path: Option<String>, graph_file: Option<String>, build: bool) {
	let contents = std::fs::read_to_string(input).unwrap();

	let mut lexer = Lexer::new(contents.chars().peekable());
	let tokens: Vec<Token> = lexer.iter().collect();

	let mut parser = Parser::new(tokens.into_iter().peekable());
	parser.parse().unwrap();

	let mut checker = TypeChecker::new(&parser);
	checker.check().unwrap();
	let ast = checker.ast;
	
	// println!("{:#?}", parser.functions);
	if let Some(path) = graph_file {
		std::fs::write(path, GraphGen::generate_graph(&ast).unwrap()).unwrap();
	}

	let compiler = Compiler::new(parser);

	let asm_output = compiler.compile();

	if let Some(output) = output_path {
		std::fs::write(&output, include_str!("nasm.asm").to_string() + &asm_output).unwrap();
		if build {
			// use bash -c because im on windows and i want it to run on wsl :-)
			Command::new("bash")
				.arg("-c")
				.arg(format!("nasm -f elf \"{0}\" -o \"{0}.o\"", output))
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

#[test]
fn run_tests() {
	let build_path = std::path::Path::new("tests/build");
	// ignore if folder already exists
	let _ = std::fs::create_dir(build_path);
	for folder in std::fs::read_dir("tests").unwrap() {
		let folder = folder.unwrap();
		if folder.path().is_dir() && folder.file_name() != "build" {
			for file in std::fs::read_dir(folder.path()).unwrap() {
				let file = file.unwrap();
				print!("{} - ", file.path().to_slash().unwrap());
				assert!(file.file_name().to_str().unwrap().to_string().ends_with(".tack"), "Unknown file in test folder: {:?}", file.file_name());
				// oops this keeps the .tack file ext.. oh well
				let binary_path = build_path.join(format!(
					"{}__{}",
					folder.file_name().to_str().unwrap(),
					file.file_name().to_str().unwrap()
				));

				run(
					file.path(),
					Some(binary_path.to_slash().unwrap().to_string()),
					None,
					true,
				);
				let out = Command::new("bash")
					.arg("-c")
					.arg(format!("./{}", binary_path.to_slash().unwrap()))
					.output()
					.unwrap();
				println!("returned code {}, output: {:?}", out.status.code().unwrap(), out.stdout);
			}
		}
	}
}
