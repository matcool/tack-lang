use colored::Colorize;
use path_slash::PathBufExt;
use std::{
	fs::File,
	io::{BufRead, BufReader, Write},
	path::{Path, PathBuf},
	process::Command,
};

use tack::run::run;

fn run_test(path: &Path, binary_path: &Path) {
	let mut expected_code = None;
	for line in BufReader::new(File::open(path).unwrap()).lines() {
		let line = line.unwrap();
		let Some((_, comment)) = line.split_once("// ") else {
			break;
		};
		let Some((key, value)) = comment.split_once(' ') else {
			break;
		};
		if key == "returns" {
			expected_code = Some(value.parse::<i32>().unwrap());
		} else {
			break;
		}
	}

	run(
		path,
		Some(binary_path.to_str().unwrap().to_string()),
		None,
		false,
		true,
	);

	let out = Command::new(std::path::absolute(binary_path).unwrap())
		.output()
		.unwrap_or_else(|_| panic!("Failed to run test {:?}", path.file_name().unwrap()));
	let code = out.status.code().unwrap();
	if !out.stdout.is_empty() {
		print!(
			" {}",
			format!("{:?}", String::from_utf8_lossy(&out.stdout)).bright_blue()
		);
	}
	if code == 11 || code < 0 {
		print!(" {}", "SEGFAULT".bright_red().italic());
	}
	if let Some(expected_code) = expected_code {
		if expected_code == code {
			print!("\r{} ", "[ OK ]".bright_green());
		} else {
			print!(
				" {}",
				format!("(expected {expected_code})")
					.bright_black()
					.italic()
			);
			print!("\r{}", "[FAIL]".bright_red());
		}
	}
}

fn main() {
	let build_path = PathBuf::from("tests/build");
	// ignore if folder already exists
	let _ = std::fs::create_dir(&build_path);
	if let Some(path) = std::env::args().nth(1) {
		run_test(
			&PathBuf::from("tests").join(path),
			&build_path.join("foo".to_string() + std::env::consts::EXE_SUFFIX),
		);
	} else {
		for folder in std::fs::read_dir("tests").unwrap() {
			let folder = folder.unwrap();
			if folder.path().is_dir() && folder.file_name() != "build" {
				for file in std::fs::read_dir(folder.path()).unwrap() {
					let file = file.unwrap();
					print!(
						"{} {}",
						"[....]".bright_black(),
						file.path().to_slash().unwrap()
					);
					_ = std::io::stdout().flush();
					let file_name = file.file_name().into_string().unwrap();
					if !file_name.ends_with(".tack") {
						println!(" - Skipping unknown file in test folder");
						continue;
					}

					let binary_path = build_path.join(format!(
						"{}__{}",
						folder.file_name().to_str().unwrap(),
						file_name.strip_suffix(".tack").unwrap().to_string()
							+ std::env::consts::EXE_SUFFIX
					));

					run_test(&file.path(), &binary_path);
					println!();
				}
			}
		}
	}
}
