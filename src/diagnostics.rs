use std::path::{Path, PathBuf};

use colored::Colorize;

use crate::span::Span;

fn span_to_line(contents: &str, span: Span) -> (usize, usize) {
	let mut line = 1;
	let mut column = 1;
	for c in contents.chars().take(span.start) {
		if c == '\n' {
			line += 1;
			column = 1;
		} else {
			column += 1;
		}
	}
	(line, column)
}

#[macro_export]
macro_rules! location {
	() => {
		Some((file!(), line!()))
	};
}

pub type ErrorOrigin = Option<(&'static str, u32)>;

fn error_at_span(
	message: &str,
	description: Option<&str>,
	span: Span,
	path: &Path,
	origin: ErrorOrigin,
	extras: &[(Span, String)],
) {
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
	// work around lyneate bug, for now
	let contents = contents.replace('\t', " ");
	let (line, column) = span_to_line(&contents, span);
	println!(
		"{} {} {message} {origin}",
		format!("{}:{}:{}:", path.to_string_lossy(), line, column).bold(),
		"error:".bright_red().bold(),
	);
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
			span.into(),
			description.unwrap_or("Here").italic().to_string(),
			(255, 100, 100),
		)]
		.into_iter()
		.chain(
			extras
				.iter()
				.map(|(span, msg)| ((*span).into(), msg.italic().to_string(), (150, 230, 255))),
		),
	)
	.with_theme(my_theme)
	.display();
}

pub trait ProducesError {
	fn file_path(&self) -> PathBuf;
	fn set_errored(&self) {}

	fn error(&self, span: Span, origin: ErrorOrigin) -> ErrorBuilder<Self> {
		ErrorBuilder {
			this: self,
			span,
			origin,
			data: Default::default(),
		}
	}
}

#[derive(Default)]
struct ErrorBuilderData {
	message: String,
	description: Option<String>,
	extras: Vec<(Span, String)>,
}

#[must_use]
pub struct ErrorBuilder<'a, T: ?Sized> {
	pub this: &'a T,
	span: Span,
	origin: ErrorOrigin,
	data: ErrorBuilderData,
}

impl<T: ProducesError> ErrorBuilder<'_, T> {
	pub fn message<S: ToString>(mut self, message: S) -> Self {
		self.data.message = message.to_string();
		self
	}

	pub fn description<S: ToString>(mut self, description: S) -> Self {
		self.data.description = Some(description.to_string());
		self
	}

	pub fn extra<S: ToString>(mut self, span: Span, message: S) -> Self {
		self.data.extras.push((span, message.to_string()));
		self
	}

	pub fn build(self) {
		error_at_span(
			&self.data.message,
			self.data.description.as_deref(),
			self.span,
			&self.this.file_path(),
			self.origin,
			&self.data.extras,
		);
		self.this.set_errored();
	}
}
