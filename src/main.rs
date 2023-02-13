use path_slash::PathBufExt;
use std::{path::PathBuf, process::Command};

use tack::run::{run, invoke_command};

fn print_help_and_exit() -> ! {
	println!(
"
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
	if cfg!(target_os = "windows") {
		match Command::new("bash").arg("--version").output() {
			Err(_) => {
				println!("Error: WSL installation not found. Please install WSL and try again.");
				std::process::exit(1)
			},
			_ => {},
		};
	}
}

fn main() {
	check_compatibility();
	println!("Compatible...");

	let input = match std::env::args().nth(1) {
		Some(value) =>
			if value == "-h" || value == "--help" { print_help_and_exit() }
			else { value },
		_ => print_help_and_exit(),
	};

	let mut output = None;
	let mut build = false;
	let mut execute = false;

	let mut iter = std::env::args().skip(2);
	while let Some(arg) = iter.next() {
		if arg == "-o" || arg == "--output" {
			output = iter.next();
		} else if arg == "-b" || arg == "--build" {
			build = true;
		} else if arg == "-r" || arg == "--run" {
			execute = true;
		} else {
			println!("Unknown option \"{arg}\"");
			print_help_and_exit();
		}
	}

	run(input, output.clone(), Some("graph.gv".into()), build);

	if output.is_some() && build && execute {
		let filename = PathBuf::from(output.unwrap()).to_slash().unwrap().to_string();
		let out = invoke_command(format!("./{}", filename));
		println!(
			"\"{filename}\" returned code {}, output: {}",
			out.status.code().unwrap(),
			String::from_utf8(out.stdout).unwrap()
		);
	}
}
