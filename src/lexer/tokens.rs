#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
	Fn,
	Let,
	Return,
	If,
	Else,
	While,
	True,
	False,
	Struct,
	Import,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Attribute {
	CExtern,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
	Assign,
	Add,
	Sub, // ☻
	Divide,
	Multiply,
	Equals,
	NotEquals,
	And,
	BitAnd,
	Dot,
	As,
	Mod,
	Or,
	BitOr,
	GreaterThan,
	LessThan,
	GreaterThanEq,
	LessThanEq,
	BitShiftLeft,
	BitShiftRight,
	// unary ops
	Not,
	Negate,
	Dereference,
	Reference,
}

impl Operator {
	pub fn is_binary(&self) -> bool {
		!matches!(
			self,
			Operator::Not | Operator::Negate | Operator::Dereference | Operator::Reference
		)
	}
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
	Keyword(Keyword),
	Identifier(String),
	Number(i64),
	Operator(Operator),
	Semicolon,
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
	LeftBracket,
	RightBracket,
	Colon,
	Comma,
	StringLiteral(String),
	Attribute(Attribute),
}

#[derive(Debug, Clone)]
pub struct Token {
	pub kind: TokenKind,
	pub span: Span,
}
