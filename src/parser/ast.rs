// ast nodes specific to the parser, not yet type checked

use crate::span::Spanned;

#[derive(Debug, Clone)]
pub enum Type {
	Name(String),
	Pointer(Box<Type>),
	Array(Box<Type>, usize),
	Unknown, // used as a default value, shouldnt be used anywhere
}

#[derive(Debug)]
pub struct ParsedStruct {
	pub name: String,
	pub fields: Vec<Variable>,
	pub functions: Vec<Function>,
}

/// Represents a variable declaration, e.g. `x: i32`
#[derive(Debug, Clone)]
pub struct Variable {
	pub name: String,
	pub ty: Type,
	pub span: Span,
}

#[derive(Debug)]
pub enum ExpressionKind {
	NumberLiteral(i64),
	BoolLiteral(bool),
	Declaration(Variable),
	Identifier(String),
	BinaryOperator(Operator, Box<Expression>, Box<Expression>),
	UnaryOperator(Operator, Box<Expression>),
	Call(String, Vec<Expression>),
	Cast(Type, Box<Expression>),
	StringLiteral(String),
	ArrayLiteral(Vec<Expression>),
	ArrayIndex(Box<Expression>, Box<Expression>),
	StructAccess(Box<Expression>, String),
	StructLiteral(String, Vec<Spanned<(String, Expression)>>),
	MethodCall(Box<Expression>, String, Vec<Expression>),
}

#[derive(Debug)]
pub struct Expression {
	pub kind: ExpressionKind,
	pub span: Span,
}

impl Expression {
	pub fn new(kind: ExpressionKind) -> Expression {
		Expression {
			kind,
			span: Default::default(),
		}
	}
}

#[derive(Debug)]
pub enum StatementKind {
	Expression(Expression),
	Return(Option<Expression>),
	If(Scope, Expression, Option<Box<Statement>>),
	While(Scope, Expression),
	Block(Scope),
}

#[derive(Debug)]
pub struct Statement {
	pub kind: StatementKind,
	pub span: Span,
}

impl Statement {
	fn new(kind: StatementKind) -> Statement {
		Statement {
			kind,
			span: Default::default(),
		}
	}

	fn requires_semicolon(&self) -> bool {
		!matches!(
			&self.kind,
			StatementKind::If(_, _, _) | StatementKind::While(_, _) | StatementKind::Block(_)
		)
	}
}

#[derive(Debug)]
pub struct Scope {
	pub statements: Vec<Statement>,
}

impl Scope {
	fn new() -> Scope {
		Scope { statements: vec![] }
	}
}

#[derive(Debug)]
pub struct Function {
	pub name: String,
	pub arguments: Vec<Variable>,
	pub return_type: Type,
	pub scope: Scope,
	pub attributes: ast::FunctionAttributes,
}

impl Function {
	fn new(name: String) -> Function {
		Function {
			name,
			arguments: vec![],
			return_type: Type::Unknown,
			scope: Scope::new(),
			attributes: Default::default(),
		}
	}
}
