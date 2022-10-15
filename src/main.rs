use path_slash::PathBufExt;
use std::{path::PathBuf, process::Command};

use tack::run::run;

fn print_help_and_exit() -> ! {
	println!(
		"tack compiler. very silly language

Usage: tack input [opts]

  input - input file to compile
  opts:
    -o output - output asm file
    -b --build - build using nasm and ld
    -r --run - run built executable (needs --build)
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
	let mut execute = false;
	let mut iter = std::env::args().skip(2);
	while let Some(arg) = iter.next() {
		if arg == "-o" {
			output = iter.next();
		} else if arg == "-b" || arg == "--build" {
			build = true;
		} else if arg == "-r" || arg == "--run" {
			execute = true;
		} else {
			println!("Unknown option \"{}\"", arg);
			print_help_and_exit();
		}
	}

	run(input, output.clone(), Some("graph.gv".into()), build);

	if output.is_some() && build && execute {
		let out = Command::new("bash")
			.arg("-c")
			.arg(format!(
				"./{}",
				PathBuf::from(output.unwrap()).to_slash().unwrap()
			))
			.output()
			.unwrap();
		println!(
			"returned code {}, output: {}",
			out.status.code().unwrap(),
			String::from_utf8(out.stdout).unwrap()
		);
	}
}
