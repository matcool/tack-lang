use std::{
	fmt::Debug,
	ops::{Deref, DerefMut},
};

#[derive(Debug, Clone, Default, Copy, PartialEq, Eq)]
pub struct Span {
	pub start: usize,
	pub end: usize,
}

impl Span {
	pub fn extended(self, other: Span) -> Span {
		Span {
			start: self.start.min(other.start),
			end: self.end.max(other.end),
		}
	}
}

/// Wraps a given type T with a span
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
	pub value: T,
	pub span: Span,
}
impl<T> Deref for Spanned<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.value
	}
}
impl<T> DerefMut for Spanned<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.value
	}
}
