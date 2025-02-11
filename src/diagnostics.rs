use std::path::{Path, PathBuf};

use colored::Colorize;

use crate::lexer::Span;

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

fn error_at_span_desc(
	message: &str,
	description: &str,
	span: Span,
	path: &Path,
	origin: ErrorOrigin,
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
	let (line, column) = span_to_line(&contents, span);
	println!(
		"{} {} {message} {origin} {:?}",
		format!("{}:{}:{}:", path.to_string_lossy(), line, column).bold(),
		"error:".bright_red().bold(),
		span
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
			span.start..span.end,
			description.italic().to_string(),
			(255, 100, 100),
		)],
	)
	.with_theme(my_theme)
	.display();
}

fn error_at_span(message: &str, span: Span, path: &Path, origin: ErrorOrigin) {
	error_at_span_desc(message, "Here", span, path, origin)
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

	pub fn build(self) {
		if let Some(description) = self.data.description {
			error_at_span_desc(
				&self.data.message,
				&description,
				self.span,
				&self.this.file_path(),
				self.origin,
			)
		} else {
			error_at_span(
				&self.data.message,
				self.span,
				&self.this.file_path(),
				self.origin,
			)
		}
		self.this.set_errored();
	}
}
