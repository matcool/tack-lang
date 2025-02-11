use std::path::Path;

use colored::Colorize;

use crate::lexer::Span;

fn span_to_offset(contents: &str, span: &Span) -> usize {
	let mut offset: usize = 0;
	let mut line = 1;
	let mut column = 1;
	for c in contents.chars() {
		if line == span.line && column == span.column {
			break;
		}
		offset += 1;
		if c == '\n' {
			column = 1;
			line += 1;
		} else if c == '\t' {
			column += 4;
		} else {
			column += 1;
		}
	}
	offset
}

pub fn error_at_span(message: &str, span: &Span, path: &Path, origin: Option<(&str, usize)>) {
	let Ok(contents) = std::fs::read_to_string(path) else {
		eprintln!(
			"Failed to read {} while trying to show error",
			path.to_string_lossy()
		);
		return;
	};
	let origin = match origin {
		Some((name, line)) => format!("(from {}:{line})", name.replace('\\', "/"))
			.truecolor(80, 80, 80)
			.italic(),
		None => "".into(),
	};
	println!(
		"{} {} {message} {origin}",
		format!("{}:{}:{}:", path.to_string_lossy(), span.line, span.column).bold(),
		"error:".bright_red().bold(),
	);
	let offset = span_to_offset(&contents, span);
	let my_theme = lyneate::Theme {
		sizing: lyneate::ThemeSizing {
			underline_spacing: 0,
			..Default::default()
		},
		..Default::default()
	};
	lyneate::Report::new_char_spanned(
		&contents,
		[(
			offset..(offset + 1),
			format!("{}", "Here".italic()),
			(255, 100, 100),
		)],
	)
	.with_theme(my_theme)
	.display();
}
