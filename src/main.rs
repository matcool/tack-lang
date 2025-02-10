use std::{path::PathBuf, process::Command};

use tack::run::run;

fn print_help_and_exit() -> ! {
	println!(
		"tack compiler. very silly language

Usage: tack <input> [options...]
       tack --help (or -h)

Options:
    -h --help			show this text and exit
    --dump				dumps the type checked AST
    -o --output (path)	built executable path
    -b --build (path)	directory to put build files
    -r --run			run built executable (requires -o)
"
	);
	std::process::exit(1);
}

fn main() {
	let mut input = None;
	let mut output = None;
	let mut dump_ast = false;
	let mut build_dir = None;
	let mut execute = false;

	let mut iter = std::env::args().skip(1);
	while let Some(arg) = iter.next() {
		if arg == "-o" || arg == "--output" {
			output = iter.next();
		} else if arg == "-b" || arg == "--build" {
			build_dir = iter.next();
		} else if arg == "-r" || arg == "--run" {
			execute = true;
		} else if arg == "--dump" {
			dump_ast = true;
		} else if arg == "-h" || arg == "--help" {
			print_help_and_exit();
		} else if input.is_none() {
			input = Some(arg);
		} else {
			println!("Unknown option \"{arg}\"");
			print_help_and_exit();
		}
	}

	let Some(input) = input else {
		eprintln!("Missing input file");
		print_help_and_exit();
	};

	run(input, output.clone(), build_dir, dump_ast, false);

	if let Some(output) = output {
		if execute {
			let output = PathBuf::from(output);
			let filename = output.file_name().unwrap().to_string_lossy();
			let status = Command::new(std::path::absolute(&output).unwrap())
				.spawn()
				.expect("Failed to spawn executable")
				.wait()
				.expect("Failed to wait");
			println!(
				"\"{filename}\" returned code {}",
				status.code().unwrap_or(-1)
			);
		}
	}
}
