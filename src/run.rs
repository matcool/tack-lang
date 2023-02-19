use std::path::PathBuf;
use std::process::{Command, Output};

use itertools::Itertools;
use path_slash::PathExt;

use crate::checker::TypeChecker;
use crate::compiler::Compiler;
use crate::graph::GraphGen;
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
	input: S,
	output_path: Option<String>,
	graph_file: Option<String>,
	build: bool,
) {
	let contents = match std::fs::read_to_string(input.clone()) {
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

	let mut checker = TypeChecker::new(&parser, input.into());
	let mut asts = checker.check().unwrap();
	asts.push(checker.ast);

	if let Some(path) = graph_file {
		if asts.len() == 1 {
			std::fs::write(path, GraphGen::generate_graph(&asts[0]).unwrap()).unwrap();
		}
	}

	if asts.len() == 1 {
		let ast = asts.into_iter().next().unwrap();

		let compiler = Compiler::new(ast);

		let asm_output = compiler.compile();

		if let Some(output) = output_path {
			std::fs::write(&output, include_str!("nasm.asm").to_string() + &asm_output).unwrap();
			if build {
				// invoke nasm and linker
				invoke_command(format!("nasm -f elf \"{0}\" -F dwarf -o \"{0}.o\"", output));
				invoke_command(format!("ld -m elf_i386 \"{0}.o\" -o \"{0}\"", output));
				if std::fs::remove_file(format!("{output}.o")).is_err() {
					eprintln!("Warning: failed to delete object file");
				}
			}
		} else {
			print!("Compiler output:\n{}", asm_output);
		}
	} else if let Some(out_dir) = output_path {
		let out_dir = PathBuf::from(out_dir);
		let mut obj_files = Vec::new();
		for ast in asts.into_iter() {
			let has_main = ast
				.functions
				.iter()
				.any(|func| func.borrow().name == "main");

			let funcs = ast
				.functions
				.iter()
				.filter_map(|func| {
					if func.borrow().is_extern {
						None
					} else {
						Some(format!("global {}", func.borrow().name))
					}
				})
				.join("\n");

			let file_name = ast
				.file_path
				.file_name()
				.unwrap()
				.to_str()
				.unwrap()
				.to_owned();

			let compiler = Compiler::new(ast);
			let asm = compiler.compile();

			let asm_file = out_dir.join(format!("{}.asm", file_name));
			let obj_file = out_dir.join(format!("{}.o", file_name));

			if has_main {
				std::fs::write(&asm_file, include_str!("nasm.asm").to_string() + &asm).unwrap();
			} else {
				std::fs::write(&asm_file, format!("section .text\n{}\n{}", funcs, &asm)).unwrap();
			}
			invoke_command(format!(
				"nasm -f elf \"{}\" -F dwarf -o \"{}\"",
				asm_file.to_slash().unwrap(),
				obj_file.to_slash().unwrap()
			));
			obj_files.push(obj_file)
		}
		invoke_command(format!(
			"ld -m elf_i386 -o \"{}\" {}",
			out_dir.join("final").to_slash().unwrap(),
			obj_files
				.iter()
				.map(|path| format!("\"{}\"", path.to_slash().unwrap()))
				.join(" ")
		));
	}
}
