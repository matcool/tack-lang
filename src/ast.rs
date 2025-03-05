use std::{
	cell::RefCell,
	collections::HashMap,
	hash::{Hash, Hasher},
	path::PathBuf,
	rc::Rc,
};

use slotmap::{new_key_type, Key, SlotMap};
use strum_macros::Display;

use crate::{lexer::Operator, span::Span};

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
	pub key: TypeKey,
	pub reference: bool,
}

impl TypeRef {
	pub const fn new(key: TypeKey) -> Self {
		Self {
			key,
			reference: false,
		}
	}

	pub fn unknown() -> Self {
		TypeKey::null().into()
	}

	pub fn is_unknown(&self) -> bool {
		self.key.is_null()
	}

	pub fn add_reference(&self) -> Self {
		Self {
			key: self.key,
			reference: true,
		}
	}

	pub fn remove_reference(&self) -> Self {
		Self {
			key: self.key,
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

impl Default for TypeRef {
	fn default() -> Self {
		Self::unknown()
	}
}

impl PartialEq for TypeRef {
	fn eq(&self, other: &TypeRef) -> bool {
		self.key == other.key
	}
}

impl Hash for TypeRef {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.key.hash(state);
	}
}

impl Eq for TypeRef {}

impl From<TypeKey> for TypeRef {
	fn from(key: TypeKey) -> Self {
		TypeRef::new(key)
	}
}

#[derive(Debug, Clone)]
pub struct Variable {
	pub name: String,
	pub unique_id: usize,
	pub ty: TypeRef,
	pub span: Span,
}

impl Variable {
	fn new_builtin(name: String, ty: TypeRef) -> Self {
		Self {
			name,
			ty,
			unique_id: 0,
			span: Default::default(),
		}
	}
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
	Call(FunctionKey, Vec<Expression>),
	Cast(Box<Expression>),
	StructAccess(Box<Expression>, String),
	ArrayLiteral(Vec<Expression>),
	ArrayIndex(Box<Expression>, Box<Expression>),
	StructLiteral(Vec<(String, Expression)>),
}

#[derive(Debug)]
pub struct Expression {
	pub kind: ExpressionKind,
	pub ty: TypeRef,
	pub span: Span,
}

impl Expression {
	pub fn new(ty: TypeRef, kind: ExpressionKind) -> Expression {
		Self::new_spanned(ty, kind, Default::default())
	}
	pub fn new_spanned(ty: TypeRef, kind: ExpressionKind, span: Span) -> Self {
		Self { kind, ty, span }
	}
	pub fn children(&self) -> Box<dyn Iterator<Item = &Expression> + '_> {
		// im sorry
		match &self.kind {
			ExpressionKind::BinaryOperator(_, a, b) => Box::new([&**a, &**b].into_iter()),
			ExpressionKind::UnaryOperator(_, a) => Box::new([&**a].into_iter()),
			ExpressionKind::Call(_, args) => Box::new(args.iter()),
			ExpressionKind::Cast(a) => Box::new([&**a].into_iter()),
			ExpressionKind::StructAccess(a, _) => Box::new([&**a].into_iter()),
			ExpressionKind::ArrayLiteral(values) => Box::new(values.iter()),
			ExpressionKind::ArrayIndex(a, b) => Box::new([&**a, &**b].into_iter()),
			ExpressionKind::StructLiteral(values) => Box::new(values.iter().map(|x| &x.1)),
			ExpressionKind::NumberLiteral(_)
			| ExpressionKind::BoolLiteral(_)
			| ExpressionKind::StringLiteral(_)
			| ExpressionKind::Declaration(_)
			| ExpressionKind::Identifier(_) => Box::new([].into_iter()),
		}
	}
	/// Wraps the expression into a cast that removes the reference
	pub fn into_cast_ref(self) -> Self {
		if self.ty.reference {
			let span = self.span;
			Expression::new_spanned(
				self.ty.remove_reference(),
				ExpressionKind::Cast(self.into()),
				span,
			)
		} else {
			self
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
	#[allow(unused)]
	pub span: Span,
}

impl Statement {
	pub fn new(kind: StatementKind, span: Span) -> Statement {
		Statement { kind, span }
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
	pub is_extern: bool,
}

#[derive(Debug, Clone)]
pub struct Function {
	pub name: String,
	pub arguments: Vec<Variable>,
	pub return_type: TypeRef,
	pub scope: Rc<Scope>,
	pub attributes: FunctionAttributes,
	pub key: FunctionKey,
	pub parent: NamespaceKey,
}

impl Function {
	pub fn new(name: String) -> Function {
		Function {
			name,
			..Default::default()
		}
	}

	pub fn is_external(&self) -> bool {
		self.attributes.is_extern || self.attributes.is_c_extern
	}
}

impl Default for Function {
	fn default() -> Self {
		Self {
			name: Default::default(),
			arguments: vec![],
			return_type: TypeRef::unknown(),
			scope: Rc::new(Scope::new(None)),
			attributes: Default::default(),
			key: Default::default(),
			parent: Default::default(),
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

#[derive(Default, Debug)]
pub struct Namespace {
	pub parent: NamespaceKey,
	pub name: String,
	pub functions: HashMap<String, FunctionKey>,
	pub children: HashMap<String, NamespaceKey>,
}

impl Namespace {
	pub fn new(name: String) -> Self {
		Self {
			name,
			..Default::default()
		}
	}
}

new_key_type! {
	pub struct FunctionKey;
	pub struct NamespaceKey;
	pub struct TypeKey;
}

#[derive(Default)]
pub struct BuiltinTypeRefs {
	pub i32: TypeRef,
	pub u8: TypeRef,
	pub bool: TypeRef,
	pub uptr: TypeRef,
	pub void: TypeRef,
	pub int_literal: TypeRef,
	pub str: TypeRef,
}

#[derive(Default)]
#[allow(clippy::upper_case_acronyms)]
pub struct AST {
	pub file_path: PathBuf,
	pub global: NamespaceKey,
	pub builtin: BuiltinTypeRefs,
	types: SlotMap<TypeKey, Type>,
	functions: SlotMap<FunctionKey, Function>,
	namespaces: SlotMap<NamespaceKey, Namespace>,
}

impl AST {
	pub fn new(file_path: PathBuf) -> Self {
		let mut namespaces = SlotMap::with_key();
		let global = namespaces.insert(Namespace::new("_".into()));
		let mut ast = Self {
			file_path,
			global,
			types: SlotMap::with_key(),
			functions: SlotMap::with_key(),
			namespaces,
			builtin: Default::default(),
		};
		ast.add_builtin_types();
		ast.add_builtin_functions();
		ast
	}

	fn add_builtin_types(&mut self) {
		self.builtin.i32 = self.add_type(Type::BuiltIn(BuiltInType::I32));
		self.builtin.u8 = self.add_type(Type::BuiltIn(BuiltInType::U8));
		self.builtin.bool = self.add_type(Type::BuiltIn(BuiltInType::Bool));
		self.builtin.uptr = self.add_type(Type::BuiltIn(BuiltInType::UPtr));
		self.builtin.void = self.add_type(Type::BuiltIn(BuiltInType::Void));
		self.builtin.int_literal = self.add_type(Type::BuiltIn(BuiltInType::IntLiteral));

		let u8_ptr = self.find_type_or_add(Type::Pointer(self.builtin.u8));
		self.builtin.str = self.add_type(Type::Struct(StructType {
			name: "str".into(),
			fields: vec![
				Variable::new_builtin("data".into(), u8_ptr),
				Variable::new_builtin("size".into(), self.builtin.i32),
			],
		}));
	}

	pub fn add_function(&mut self, parent: NamespaceKey, mut function: Function) -> FunctionKey {
		let name = function.name.clone();
		function.parent = parent;
		let key = self.functions.insert_with_key(move |key| {
			function.key = key;
			function
		});
		let ns = &mut self.namespaces[parent];
		ns.functions.insert(name, key);
		key
	}

	pub fn get_function(&self, key: FunctionKey) -> &Function {
		self.functions
			.get(key)
			.expect("Tried to get missing function, internal error!")
	}

	pub fn set_function(&mut self, key: FunctionKey, function: Function) {
		self.functions[key] = function;
	}

	pub fn iter_functions(&self) -> impl Iterator<Item = &Function> {
		self.functions.values()
	}

	pub fn add_namespace(&mut self, parent: NamespaceKey, mut ns: Namespace) -> NamespaceKey {
		ns.parent = parent;
		let name = ns.name.clone();
		let key = self.namespaces.insert(ns);
		if let Some(parent) = self.namespaces.get_mut(parent) {
			parent.children.insert(name, key);
		}
		key
	}

	pub fn get_namespace(&self, key: NamespaceKey) -> &Namespace {
		self.namespaces
			.get(key)
			.expect("Tried to get missing namespace, internal error!")
	}

	fn add_builtin_functions(&mut self) {
		let void_ptr = self.find_type_or_add(Type::Pointer(self.builtin.void));
		self.add_function(
			self.global,
			Function {
				name: "tack_malloc".into(),
				arguments: vec![Variable::new_builtin("size".into(), self.builtin.uptr)],
				return_type: void_ptr,
				attributes: FunctionAttributes {
					is_c_extern: true,
					..Default::default()
				},
				..Default::default()
			},
		);
		self.add_function(
			self.global,
			Function {
				name: "tack_free".into(),
				arguments: vec![Variable::new_builtin("ptr".into(), void_ptr)],
				return_type: self.builtin.void,
				attributes: FunctionAttributes {
					is_c_extern: true,
					..Default::default()
				},
				..Default::default()
			},
		);
		self.add_function(
			self.global,
			Function {
				name: "tack_memcpy".into(),
				arguments: vec![
					Variable::new_builtin("dst".into(), void_ptr),
					Variable::new_builtin("src".into(), void_ptr),
					Variable::new_builtin("size".into(), self.builtin.uptr),
				],
				return_type: void_ptr,
				attributes: FunctionAttributes {
					is_c_extern: true,
					..Default::default()
				},
				..Default::default()
			},
		);
		self.add_function(
			self.global,
			Function {
				name: "tack_print".into(),
				arguments: vec![Variable::new_builtin("str".into(), self.builtin.str)],
				return_type: self.builtin.void,
				attributes: FunctionAttributes {
					is_c_extern: true,
					..Default::default()
				},
				..Default::default()
			},
		);
	}

	pub fn find_type<P: FnMut(&&Type) -> bool>(&self, mut predicate: P) -> Option<TypeRef> {
		self.types
			.iter()
			.find(move |(_, v)| predicate(v))
			.map(|(key, _)| key.into())
	}

	pub fn find_type_or_add(&mut self, ty: Type) -> TypeRef {
		self.find_type(|&t| t == &ty)
			.unwrap_or_else(|| self.add_type(ty))
	}

	pub fn find_type_by_name(&self, name: &str) -> Option<TypeRef> {
		self.find_type(|&t| name == t.name(self))
	}

	pub fn add_type(&mut self, ty: Type) -> TypeRef {
		self.types.insert(ty).into()
	}

	pub fn get_type(&self, type_ref: TypeRef) -> &Type {
		self.types
			.get(type_ref.key)
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

	pub fn iter_structs(&self) -> impl Iterator<Item = &StructType> {
		self.types.values().filter_map(|s| match s {
			Type::Struct(s) => Some(s),
			_ => None,
		})
	}

	/// Imports structs and functions from another ast, marking them as external
	pub fn import_ast(&mut self, old_ast: &AST) {
		self.import_namespace(old_ast, &old_ast.namespaces[old_ast.global], self.global);

		// import structs since those arent stored in the namespace yet..
		for (key, ty) in old_ast.types.iter().by_ref() {
			if !matches!(ty, Type::Struct(_)) {
				continue;
			}
			if self.find_type(|t| t == &ty).is_none() {
				self.import_type_from(old_ast, key.into());
			}
		}
	}

	fn import_type_from(&mut self, old_ast: &AST, type_ref: TypeRef) -> TypeRef {
		match old_ast.get_type(type_ref).clone() {
			k @ Type::BuiltIn(_) => self.find_type_or_add(k),
			Type::Pointer(type_ref) => {
				let inner = self.import_type_from(old_ast, type_ref);
				self.find_type_or_add(Type::Pointer(inner))
			}
			Type::Array(type_ref, size) => {
				let inner = self.import_type_from(old_ast, type_ref);
				self.find_type_or_add(Type::Array(inner, size))
			}
			Type::Struct(mut struct_type) => {
				for var in &mut struct_type.fields {
					var.ty = self.import_type_from(old_ast, var.ty);
				}
				self.find_type_or_add(Type::Struct(struct_type))
			}
		}
	}

	fn import_namespace_functions(
		&mut self,
		old_ast: &AST,
		old_ns: &Namespace,
		new_ns: NamespaceKey,
	) {
		for function in old_ns.functions.values() {
			let function = &old_ast.functions[*function];
			if function.is_external() {
				continue;
			}
			let mut imported_func = function.clone();
			imported_func.attributes.is_extern = true;
			for arg in &mut imported_func.arguments {
				arg.ty = self.import_type_from(old_ast, arg.ty);
			}
			imported_func.return_type = self.import_type_from(old_ast, imported_func.return_type);
			self.add_function(new_ns, imported_func);
		}
	}

	fn import_namespace(&mut self, old_ast: &AST, old_ns: &Namespace, new_ns: NamespaceKey) {
		self.import_namespace_functions(old_ast, old_ns, new_ns);
		for old_child in old_ns.children.values() {
			let old_child = &old_ast.namespaces[*old_child];
			let new_child = self.add_namespace(new_ns, Namespace::new(old_child.name.clone()));
			self.import_namespace(old_ast, old_child, new_child);
		}
	}
}

pub trait HasAST {
	fn ast(&self) -> &AST;

	fn format_type(&self, ty: TypeRef) -> String {
		ty.formatted(self.ast())
	}
}
