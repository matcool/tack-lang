use path_slash::PathBufExt;
use std::{
	io::{BufRead, BufReader},
	path::PathBuf,
	process::Command,
};

use tack::run::run;

fn run_test(path: PathBuf, binary_path: PathBuf) {
	let mut expected_code = None;
	for line in BufReader::new(std::fs::File::open(path.clone()).unwrap()).lines() {
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
		Some(binary_path.to_slash().unwrap().to_string()),
		None,
		true,
	);

	let out = Command::new("bash")
		.arg("-c")
		.arg(format!("./{}", binary_path.to_slash().unwrap()))
		.output()
		.unwrap();
	let code = out.status.code().unwrap();
	print!("returned code {code} ");
	if !out.stdout.is_empty() {
		print!(" output: {:?} ", out.stdout);
	}
	if code == 11 {
		print!("SEGFAULT");
	} else if let Some(expected_code) = expected_code {
		if expected_code == code {
			print!("OK");
		} else {
			print!("FAIL (expected {expected_code})");
		}
	}
}

fn main() {
	let build_path = std::path::Path::new("tests/build");
	// ignore if folder already exists
	let _ = std::fs::create_dir(build_path);
	if let Some(path) = std::env::args().nth(1) {
		run_test(
			std::path::Path::new("tests").join(path),
			build_path.join("foo"),
		);
	} else {
		for folder in std::fs::read_dir("tests").unwrap() {
			let folder = folder.unwrap();
			if folder.path().is_dir() && folder.file_name() != "build" {
				for file in std::fs::read_dir(folder.path()).unwrap() {
					let file = file.unwrap();
					print!("{} - ", file.path().to_slash().unwrap());
					let file_name = file.file_name().into_string().unwrap();
					if !file_name.ends_with(".tack") {
						println!("Skipping unknown file in test folder");
						continue;
					}

					let binary_path = build_path.join(format!(
						"{}__{}",
						folder.file_name().to_str().unwrap(),
						file_name.strip_suffix(".tack").unwrap()
					));

					run_test(file.path(), binary_path);
					println!();
				}
			}
		}
	}
}
