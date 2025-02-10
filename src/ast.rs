use std::{
	cell::RefCell,
	hash::{Hash, Hasher},
	path::PathBuf,
	rc::Rc,
};

use itertools::Itertools;
use strum_macros::Display;

use crate::lexer::{Operator, Span};

#[derive(Debug, PartialEq, Clone)]
pub enum BuiltInType {
	I32,
	U8,
	UPtr,
	Bool,
	Void,
	IntLiteral,
}

#[derive(Debug, Clone)]
pub struct StructType {
	pub name: String,
	pub fields: Vec<Variable>,
}

impl PartialEq for StructType {
	fn eq(&self, other: &Self) -> bool {
		// is this a good idea?
		self.name == other.name
	}
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
	BuiltIn(BuiltInType),
	Pointer(TypeRef),
	Struct(StructType),
	Array(TypeRef, usize),
}

#[derive(Debug, Clone, Copy)]
pub struct TypeRef {
	pub id: usize,
	pub reference: bool,
}

impl TypeRef {
	pub const fn new(id: usize) -> Self {
		Self {
			id,
			reference: false,
		}
	}

	pub fn unknown() -> Self {
		Self::new(usize::MAX)
	}

	pub fn is_unknown(&self) -> bool {
		self.id == usize::MAX
	}

	pub fn add_reference(&self) -> Self {
		Self {
			id: self.id,
			reference: true,
		}
	}

	pub fn remove_reference(&self) -> Self {
		Self {
			id: self.id,
			reference: false,
		}
	}

	pub fn formatted(&self, ast: &AST) -> String {
		let mut name = String::from("??");
		if !self.is_unknown() {
			let ty = ast.get_type(*self);
			name = ty.name(ast);
		}
		if self.reference {
			name.push('&');
		}
		name
	}
}

impl PartialEq for TypeRef {
	fn eq(&self, other: &TypeRef) -> bool {
		self.id == other.id
	}
}

impl Hash for TypeRef {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.id.hash(state);
	}
}

impl Eq for TypeRef {}

#[derive(Debug, Clone)]
pub struct Variable {
	pub name: String,
	pub unique_id: usize,
	pub ty: TypeRef,
}

#[derive(Debug, Display)]
pub enum ExpressionKind {
	NumberLiteral(i64),
	BoolLiteral(bool),
	StringLiteral(String),
	Declaration(Variable),
	Identifier(Variable),
	BinaryOperator(Operator, Box<Expression>, Box<Expression>),
	UnaryOperator(Operator, Box<Expression>),
	// TODO: should just use child expression instead of function name
	Call(String, Vec<Expression>),
	Cast(Box<Expression>),
	StructAccess(Box<Expression>, String),
	ArrayLiteral(Vec<Expression>),
	ArrayIndex(Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub struct Expression {
	pub kind: ExpressionKind,
	pub value_type: TypeRef,
	pub span: Span,
}

impl Expression {
	pub fn new(ty: TypeRef, kind: ExpressionKind) -> Expression {
		Self::new_spanned(ty, kind, Default::default())
	}
	pub fn new_spanned(ty: TypeRef, kind: ExpressionKind, span: Span) -> Self {
		Self {
			kind,
			value_type: ty,
			span,
		}
	}
	pub fn list_children(&self) -> Box<[&Expression]> {
		match &self.kind {
			ExpressionKind::BinaryOperator(_, a, b) => [&**a, &**b].into(),
			ExpressionKind::UnaryOperator(_, a) => [&**a].into(),
			ExpressionKind::Call(_, args) => args.iter().collect(),
			ExpressionKind::Cast(a) => [&**a].into(),
			ExpressionKind::StructAccess(a, _) => [&**a].into(),
			ExpressionKind::ArrayLiteral(values) => values.iter().collect(),
			ExpressionKind::ArrayIndex(a, b) => [&**a, &**b].into(),
			_ => [].into(),
		}
	}
}

#[derive(Debug)]
pub enum StatementKind {
	Expression(Expression),
	Return(Option<Expression>),
	If(Rc<Scope>, Expression, Option<Box<Statement>>),
	While(Rc<Scope>, Expression),
	Block(Rc<Scope>),
}

#[derive(Debug)]
pub struct Statement {
	pub kind: StatementKind,
}

impl Statement {
	pub fn new(kind: StatementKind) -> Statement {
		Statement { kind }
	}
}

pub struct Scope {
	pub parent: Option<Rc<Scope>>,
	// use refcell since these might still be modified
	// even though the scope is referenced in a child scope
	pub statements: RefCell<Vec<Statement>>,
	pub variables: RefCell<Vec<Variable>>,
	var_counter: RefCell<usize>,
}

impl Scope {
	pub fn new(parent: Option<Rc<Scope>>) -> Scope {
		Scope {
			parent,
			statements: vec![].into(),
			variables: vec![].into(),
			var_counter: 1.into(),
		}
	}

	pub fn add_statement(&self, statement: Statement) {
		self.statements.borrow_mut().push(statement);
	}

	fn next_var_counter(&self) -> usize {
		if let Some(parent) = &self.parent {
			parent.next_var_counter()
		} else {
			self.var_counter.replace_with(|&mut prev| prev + 1)
		}
	}

	/// Adds a variable into the scope. Returned `Variable` contains a set unique_id.
	pub fn add_variable(&self, mut variable: Variable) -> Variable {
		variable.unique_id = self.next_var_counter();
		self.variables.borrow_mut().push(variable.clone());
		variable
	}

	fn find_variable(&self, name: &str) -> Option<Variable> {
		self.variables
			.borrow()
			.iter()
			.rev()
			.find(|var| var.name == name)
			.cloned()
	}

	pub fn find_variable_recursive(&self, name: &str) -> Option<Variable> {
		if let Some(var) = self.find_variable(name) {
			return Some(var);
		} else if let Some(var) = self
			.parent
			.clone()
			.and_then(|s| s.find_variable_recursive(name))
		{
			return Some(var);
		}
		None
	}
}

impl std::fmt::Debug for Scope {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Scope")
			.field(
				"parent",
				self.parent.as_ref().map_or(&"None", |_| &"Some(...)"),
			)
			.field("statements", &self.statements)
			.field("variables", &self.variables)
			.finish()
	}
}

#[derive(Debug, Clone, Default)]
pub struct FunctionAttributes {
	pub is_c_extern: bool,
}

#[derive(Debug, Clone)]
pub struct Function {
	pub name: String,
	pub arguments: Vec<Variable>,
	pub return_type: TypeRef,
	pub scope: Rc<Scope>,
	pub attributes: FunctionAttributes,
}

impl Function {
	pub fn new(name: String) -> Function {
		Function {
			name,
			arguments: vec![],
			return_type: TypeRef::unknown(),
			scope: Rc::new(Scope::new(None)),
			attributes: Default::default(),
		}
	}
}

impl Type {
	pub fn name(&self, ast: &AST) -> String {
		match self {
			Type::BuiltIn(builtin) => match builtin {
				BuiltInType::I32 => "i32".into(),
				BuiltInType::U8 => "u8".into(),
				BuiltInType::UPtr => "uptr".into(),
				BuiltInType::Bool => "bool".into(),
				BuiltInType::Void => "void".into(),
				BuiltInType::IntLiteral => "<int>".into(),
			},
			Type::Pointer(inner) => format!("{}*", ast.get_type(*inner).name(ast)),
			Type::Struct(str) => str.name.clone(),
			Type::Array(inner, size) => format!("{}[{size}]", ast.get_type(*inner).name(ast)),
		}
	}
}

#[derive(Default)]
pub struct AST {
	pub functions: Vec<Function>,
	pub types: Vec<Type>,
	pub file_path: PathBuf,
}

pub const BUILTIN_TYPE_I32: TypeRef = TypeRef::new(0);
pub const BUILTIN_TYPE_U8: TypeRef = TypeRef::new(1);
pub const BUILTIN_TYPE_BOOL: TypeRef = TypeRef::new(2);
#[allow(unused)]
pub const BUILTIN_TYPE_UPTR: TypeRef = TypeRef::new(3);
pub const BUILTIN_TYPE_VOID: TypeRef = TypeRef::new(4);
pub const BUILTIN_TYPE_INT_LITERAL: TypeRef = TypeRef::new(5);
pub const BUILTIN_TYPE_STR: TypeRef = TypeRef::new(7); // 6 is u8*

impl AST {
	pub fn new(file_path: PathBuf) -> Self {
		let mut ast = Self {
			file_path,
			..Default::default()
		};
		ast.add_builtin_types();
		ast.add_builtin_functions();
		ast
	}

	fn add_builtin_types(&mut self) {
		self.add_type(Type::BuiltIn(BuiltInType::I32));
		self.add_type(Type::BuiltIn(BuiltInType::U8));
		self.add_type(Type::BuiltIn(BuiltInType::Bool));
		self.add_type(Type::BuiltIn(BuiltInType::UPtr));
		self.add_type(Type::BuiltIn(BuiltInType::Void));
		self.add_type(Type::BuiltIn(BuiltInType::IntLiteral));

		let u8_ptr = self.find_type_or_add(Type::Pointer(BUILTIN_TYPE_U8));
		self.add_type(Type::Struct(StructType {
			name: "str".into(),
			fields: vec![
				Variable {
					name: "data".into(),
					unique_id: 0,
					ty: u8_ptr,
				},
				Variable {
					name: "size".into(),
					unique_id: 0,
					ty: BUILTIN_TYPE_I32,
				},
			],
		}));
	}

	fn add_builtin_functions(&mut self) {
		let void_ptr = self.find_type_or_add(Type::Pointer(BUILTIN_TYPE_VOID));
		self.functions.push(Function {
			name: "tack_malloc".into(),
			arguments: vec![Variable {
				name: "size".into(),
				ty: BUILTIN_TYPE_UPTR,
				unique_id: 0,
			}],
			return_type: void_ptr,
			attributes: FunctionAttributes { is_c_extern: true },
			scope: Scope::new(None).into(),
		});
		self.functions.push(Function {
			name: "tack_free".into(),
			arguments: vec![Variable {
				name: "ptr".into(),
				ty: void_ptr,
				unique_id: 0,
			}],
			return_type: BUILTIN_TYPE_VOID,
			attributes: FunctionAttributes { is_c_extern: true },
			scope: Scope::new(None).into(),
		});
		self.functions.push(Function {
			name: "tack_memcpy".into(),
			arguments: vec![
				Variable {
					name: "dst".into(),
					ty: void_ptr,
					unique_id: 0,
				},
				Variable {
					name: "src".into(),
					ty: void_ptr,
					unique_id: 0,
				},
				Variable {
					name: "size".into(),
					ty: BUILTIN_TYPE_UPTR,
					unique_id: 0,
				},
			],
			return_type: void_ptr,
			attributes: FunctionAttributes { is_c_extern: true },
			scope: Scope::new(None).into(),
		});
		self.functions.push(Function {
			name: "tack_print".into(),
			arguments: vec![Variable {
				name: "str".into(),
				ty: BUILTIN_TYPE_STR,
				unique_id: 0,
			}],
			return_type: BUILTIN_TYPE_VOID,
			attributes: FunctionAttributes { is_c_extern: true },
			scope: Scope::new(None).into(),
		});
	}

	fn find_type<P: FnMut(&&Type) -> bool>(&self, predicate: P) -> Option<TypeRef> {
		self.types
			.iter()
			.find_position(predicate)
			.map(|(id, _)| TypeRef::new(id))
	}

	pub fn find_type_or_add(&mut self, ty: Type) -> TypeRef {
		self.find_type(|&t| t == &ty)
			.unwrap_or_else(|| self.add_type(ty))
	}

	pub fn find_type_by_name(&self, name: &str) -> Option<TypeRef> {
		self.find_type(|&t| name == t.name(self))
	}

	pub fn add_type(&mut self, ty: Type) -> TypeRef {
		self.types.push(ty);
		TypeRef::new(self.types.len() - 1)
	}

	pub fn get_type(&self, type_ref: TypeRef) -> &Type {
		self.types
			.get(type_ref.id)
			.expect("invalid type id passed into get_type")
	}

	pub fn is_array(&self, type_ref: TypeRef) -> bool {
		matches!(self.get_type(type_ref), Type::Array(..))
	}

	pub fn is_pointer(&self, type_ref: TypeRef) -> bool {
		matches!(self.get_type(type_ref), Type::Pointer(_))
	}

	pub fn is_struct(&self, type_ref: TypeRef) -> bool {
		matches!(self.get_type(type_ref), Type::Struct(_))
	}

	pub fn is_integer(&self, type_ref: TypeRef) -> bool {
		matches!(
			self.get_type(type_ref),
			Type::BuiltIn(BuiltInType::I32)
				| Type::BuiltIn(BuiltInType::U8)
				| Type::BuiltIn(BuiltInType::UPtr)
		)
	}
}

impl Expression {
	/// Turns the expression into a cast into the given type
	fn replace_with_cast(&mut self, ty: TypeRef) {
		let mut cast = Expression::new(ty, ExpressionKind::NumberLiteral(0));
		std::mem::swap(self, &mut cast);
		// self is now the cast
		self.kind = ExpressionKind::Cast(cast.into());
	}

	/// Turns the expression into a cast that removes the reference
	pub fn cast_if_reference(&mut self) {
		if self.value_type.reference {
			self.replace_with_cast(self.value_type.remove_reference());
		}
	}
}
