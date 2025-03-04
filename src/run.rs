use std::path::PathBuf;
use std::process::Command;

use crate::checker::TypeChecker;
use crate::compiler::Compiler;
use crate::lexer::*;
use crate::parser::Parser;

pub fn run<S: AsRef<std::path::Path> + Into<PathBuf> + Clone>(
	input_path: S,
	output_path: Option<String>,
	build_dir: Option<String>,
	dump_ast: bool,
	quiet: bool,
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

	let mut parser = Parser::new(tokens.into_iter().peekable(), input_path.clone().into());
	parser.parse().unwrap();

	let asts = TypeChecker::new(input_path.into()).check(parser).unwrap();
	let ast = &asts[0];

	if dump_ast {
		crate::dump::dump(ast);
		return;
	}
	let compiled_asts: Vec<_> = asts
		.into_iter()
		.map(|ast| (ast.file_path.clone(), Compiler::new(&ast).compile()))
		.collect();

	if output_path.is_none() && build_dir.is_none() {
		println!("{}", compiled_asts[0].1);
		return;
	}
	if output_path.is_none() {
		panic!("Output path must be set to build");
	}
	let output_path = output_path.unwrap();
	let build_dir = build_dir
		.map(PathBuf::from)
		.unwrap_or_else(|| std::env::temp_dir().join("tack_build"));

	std::fs::create_dir_all(&build_dir).expect("Could not create build dir");
	let inputs = compiled_asts
		.iter()
		.map(|(path, c_code)| {
			let path = build_dir.join(
				path.file_name()
					.expect("Expected filename")
					.to_str()
					.unwrap()
					.to_string() + ".c",
			);
			std::fs::write(&path, c_code).expect("Unable to write compiled code");
			path
		})
		.collect::<Vec<_>>();
	let runtime_dir = build_dir.join("runtime");
	std::fs::create_dir_all(&runtime_dir).expect("Could not create runtime dir");
	// very hacky but whatever
	std::fs::write(
		runtime_dir.join("tack_runtime.c"),
		include_str!("../runtime/c/tack_runtime.c"),
	)
	.expect("Unable to write runtime");
	std::fs::write(
		runtime_dir.join("tack_runtime.h"),
		include_str!("../runtime/c/tack_runtime.h"),
	)
	.expect("Unable to write runtime");
	// finally, run c compiler
	let c_compiler = std::env::var("CC").unwrap_or("clang".to_string());
	let mut command = Command::new(c_compiler);
	for input in inputs {
		command.arg(input);
	}
	command.arg("-I").arg(&runtime_dir);
	command.arg(runtime_dir.join("tack_runtime.c"));
	command.arg("-o").arg(output_path);
	if !quiet {
		println!("Executing command: {:?}", command);
	}
	let status = if quiet {
		command
			.output()
			.expect("Failed to run command (is clang missing?)")
			.status
	} else {
		command
			.spawn()
			.expect("Failed to run command (is clang missing?)")
			.wait()
			.expect("Failed to wait for process")
	};
	if !status.success() {
		eprintln!("Failed to compile");
		std::process::exit(1);
	}
}
