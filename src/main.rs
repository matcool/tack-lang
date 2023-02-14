use path_slash::PathBufExt;
use std::{path::PathBuf, process::Command};

use tack::run::{invoke_command, run};

fn print_help_and_exit() -> ! {
	println!(
		"tack compiler. very silly language

Usage: tack <input> [options...] [output]
       tack --help (or -h)

Options:
    -h --help	  	show this text and exit
    -o --output   	output asm file
    -b --build 	  	build input using nasm and ld
    -r --run 	  	run built executable (requires --build)
"
	);
	std::process::exit(1);
}

fn check_compatibility() {
	if cfg!(windows) && Command::new("bash").arg("--version").output().is_err() {
		println!("Error: WSL installation not found. Please install WSL and try again.");
		std::process::exit(1)
	}
}

fn main() {
	check_compatibility();

	let mut input = None;
	let mut output = None;
	let mut build = false;
	let mut execute = false;

	let mut iter = std::env::args().skip(1);
	while let Some(arg) = iter.next() {
		if arg == "-o" || arg == "--output" {
			output = iter.next();
		} else if arg == "-b" || arg == "--build" {
			build = true;
		} else if arg == "-r" || arg == "--run" {
			execute = true;
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

	run(input, output.clone(), Some("graph.gv".into()), build);

	if let Some(output) = output {
		if build && execute {
			let filename = PathBuf::from(output).to_slash().unwrap().to_string();
			let out = invoke_command(format!("./{}", filename));
			println!(
				"\"{filename}\" returned code {}, output:\n{}",
				out.status.code().unwrap(),
				String::from_utf8(out.stdout).unwrap()
			);
		}
	}
}
