use path_slash::PathBufExt;
use std::process::Command;

use tack::run::run;

fn main() {
	let build_path = std::path::Path::new("tests/build");
	// ignore if folder already exists
	let _ = std::fs::create_dir(build_path);
	for folder in std::fs::read_dir("tests").unwrap() {
		let folder = folder.unwrap();
		if folder.path().is_dir() && folder.file_name() != "build" {
			for file in std::fs::read_dir(folder.path()).unwrap() {
				let file = file.unwrap();
				print!("{} - ", file.path().to_slash().unwrap());
				assert!(
					file.file_name()
						.to_str()
						.unwrap()
						.to_string()
						.ends_with(".tack"),
					"Unknown file in test folder: {:?}",
					file.file_name()
				);
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
				let code = out.status.code().unwrap();
				print!(
					"returned code {}, output: {:?} ",
					code,
					out.stdout
				);
				if code == 11 {
					print!("SEGFAULT");
				}
				println!();
			}
		}
	}
}
