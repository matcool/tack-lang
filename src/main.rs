use tack::run::run;

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
