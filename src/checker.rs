use std::{borrow::BorrowMut, cell::RefCell, rc::Rc};

use itertools::Itertools;

use crate::{
	lexer::Operator,
	parser::{
		BuiltInType, Expression, ExpressionKind, Function, ParsedType, ParsedVariable, Parser,
		Scope, Statement, StatementKind, StructType, Type, TypeRef, Variable,
	},
};

impl TypeRef {
	fn add_reference(&self) -> Self {
		Self {
			id: self.id,
			reference: true,
		}
	}

	fn remove_reference(&self) -> Self {
		Self {
			id: self.id,
			reference: false,
		}
	}
}

impl PartialEq for TypeRef {
	fn eq(&self, other: &TypeRef) -> bool {
		self.id == other.id
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
			Type::Pointer(inner) => format!("{}*", ast.get_type(*inner).name(&ast)),
			Type::Struct(str) => str.name.clone(),
			Type::Array(inner, size) => format!("{}[{size}]", ast.get_type(*inner).name(&ast)),
		}
	}
}

pub struct AST {
	pub functions: Vec<Function>,
	pub types: Vec<Rc<Type>>,
}

impl AST {
	pub fn new() -> Self {
		Self {
			functions: vec![],
			types: vec![],
		}
	}

	fn find_type<P: FnMut(&&Rc<Type>) -> bool>(&self, predicate: P) -> Option<TypeRef> {
		self.types
			.iter()
			.find_position(predicate)
			.map(|(id, _)| TypeRef::new(id))
	}

	fn find_type_or_add(&mut self, ty: Type) -> TypeRef {
		self.find_type(|&t| t.as_ref() == &ty)
			.unwrap_or_else(|| self.add_type(ty))
	}

	fn find_type_by_name(&self, name: &String) -> Option<TypeRef> {
		self.find_type(|&t| name == &t.name(&self))
	}

	fn add_type(&mut self, ty: Type) -> TypeRef {
		self.types.push(Rc::new(ty));
		TypeRef::new(self.types.len() - 1)
	}

	pub fn get_type(&self, type_ref: TypeRef) -> Rc<Type> {
		Rc::clone(
			self.types
				.get(type_ref.id)
				.expect("invalid type id passed into get_type"),
		)
	}

	pub fn get_type_size(&self, type_ref: TypeRef) -> usize {
		self.get_type(type_ref).size(self)
	}

	pub fn is_struct_or_array(&self, type_ref: TypeRef) -> bool {
		matches!(
			self.get_type(type_ref).as_ref(),
			Type::Struct(_) | Type::Array(..)
		)
	}

	pub fn is_pointer(&self, type_ref: TypeRef) -> bool {
		matches!(self.get_type(type_ref).as_ref(), Type::Pointer(_))
	}

	pub fn is_integer(&self, type_ref: TypeRef) -> bool {
		matches!(
			self.get_type(type_ref).as_ref(),
			Type::BuiltIn(BuiltInType::I32)
				| Type::BuiltIn(BuiltInType::U8)
				| Type::BuiltIn(BuiltInType::UPtr)
		)
	}
}

impl Expression {
	fn replace_with(&mut self, mut new: Expression) {
		std::mem::swap(self, &mut new);
	}

	// fn replace_with_children(&mut self, mut new: Expression) {
	// 	std::mem::swap(self, &mut new);
	// 	self.children.append(&mut new.children);
	// }

	fn replace_with_cast(&mut self, typ: TypeRef) {
		let mut cast = Expression::new(ExpressionKind::Cast, vec![]);
		cast.value_type = typ;
		std::mem::swap(self, &mut cast);
		// self and new have swapped
		// so this would actually be cast.children.push(self)
		self.children.push(cast);
	}

	fn cast_if_reference(&mut self) {
		if self.value_type.reference {
			self.replace_with_cast(self.value_type.remove_reference());
		}
	}
}

#[derive(Debug)]
pub enum TypeCheckerError {
	TypeMismatch(String),
	VariableNotFound(String),
	// VariableAlreadyExists(String),
	FieldAlreadyExists(String),
	FunctionNotFound(String),
	ArgumentCountMismatch, // great name
	InvalidReference,
	InvalidDereference,
	UnknownType(String),
	InvalidCast,
	InvalidOperand,
}

const BUILTIN_TYPE_I32: TypeRef = TypeRef::new(0);
const BUILTIN_TYPE_U8: TypeRef = TypeRef::new(1);
const BUILTIN_TYPE_BOOL: TypeRef = TypeRef::new(2);
#[allow(unused)]
const BUILTIN_TYPE_UPTR: TypeRef = TypeRef::new(3);
const BUILTIN_TYPE_VOID: TypeRef = TypeRef::new(4);
const BUILTIN_TYPE_INT_LITERAL: TypeRef = TypeRef::new(5);
const BUILTIN_TYPE_STR: TypeRef = TypeRef::new(7); // 6 is u8*

pub struct TypeChecker<'a> {
	parser: &'a Parser,
	pub ast: AST,
}

impl TypeChecker<'_> {
	pub fn new(parser: &'_ Parser) -> TypeChecker {
		TypeChecker {
			parser,
			ast: AST::new(),
		}
	}

	pub fn check(&mut self) -> Result<(), TypeCheckerError> {
		self.ast.add_type(Type::BuiltIn(BuiltInType::I32));
		self.ast.add_type(Type::BuiltIn(BuiltInType::U8));
		self.ast.add_type(Type::BuiltIn(BuiltInType::Bool));
		self.ast.add_type(Type::BuiltIn(BuiltInType::UPtr));
		self.ast.add_type(Type::BuiltIn(BuiltInType::Void));
		self.ast.add_type(Type::BuiltIn(BuiltInType::IntLiteral));

		let u8_ptr = self.ast.find_type_or_add(Type::Pointer(BUILTIN_TYPE_U8));
		self.ast.add_type(Type::Struct(StructType {
			name: "str".into(),
			fields: vec![
				Variable {
					name: "data".into(),
					ty: u8_ptr,
				},
				Variable {
					name: "size".into(),
					ty: BUILTIN_TYPE_I32,
				},
			],
		}));

		for parsed_struct in &self.parser.parsed_structs {
			let mut fields: Vec<Variable> = vec![];
			for field in &parsed_struct.fields {
				if fields.iter().any(|f| f.name == field.name) {
					return Err(TypeCheckerError::FieldAlreadyExists(field.name.clone()));
				}
				fields.push(self.check_parsed_var(field)?);
			}
			self.ast.add_type(Type::Struct(StructType {
				name: parsed_struct.name.clone(),
				fields,
			}));
		}

		for function in &self.parser.functions {
			self.check_function(function)?;
		}
		Ok(())
	}

	fn check_parsed_var(
		&mut self,
		parsed_var: &ParsedVariable,
	) -> Result<Variable, TypeCheckerError> {
		Ok(Variable {
			name: parsed_var.name.clone(),
			ty: self.check_parsed_type(&parsed_var.ty)?,
		})
	}

	fn check_function(&mut self, function: &RefCell<Function>) -> Result<(), TypeCheckerError> {
		// check function.parsed_return_type into function.return_type
		{
			let mut function = function.borrow_mut();
			function.return_type = self.check_parsed_type(&function.parsed_return_type)?;
			function.is_struct_return = self.ast.is_struct_or_array(function.return_type);

			let mut args = Vec::new();
			for arg in &function.parsed_arguments {
				args.push(self.check_parsed_var(arg)?);
			}
			function.arguments = args;
		}
		let function = function.borrow();
		self.check_scope(Rc::clone(&function.scope), &function)?;
		// mm yummy clone!
		self.ast.functions.push(function.clone());
		Ok(())
	}

	fn check_scope(
		&mut self,
		scope: Rc<Scope>,
		function: &Function,
	) -> Result<(), TypeCheckerError> {
		if !Rc::ptr_eq(&scope, &function.scope) {
			function.scope.children.borrow_mut().push(Rc::clone(&scope));
		}
		for statement in &scope.statements {
			self.check_statement(&mut statement.borrow_mut(), function, Rc::clone(&scope))?;
		}
		Ok(())
	}

	fn format_type(&self, type_ref: TypeRef) -> String {
		let mut name = String::from("??");
		if !type_ref.is_unknown() {
			let ty = self.ast.get_type(type_ref);
			name = ty.name(&self.ast);
		}
		if type_ref.reference {
			name.push('&');
		}
		name
	}

	fn check_statement(
		&mut self,
		statement: &mut Statement,
		function: &Function,
		scope: Rc<Scope>,
	) -> Result<(), TypeCheckerError> {
		match statement.kind {
			StatementKind::Return => {
				self.check_expression(&mut statement.children[0], function, scope)?;

				let ty =
					self.promote_int_literal_into(&mut statement.children[0], function.return_type);

				if ty != function.return_type {
					return Err(TypeCheckerError::TypeMismatch(
						"between return type and the expression".into(),
					));
				}
				if !function.return_type.reference && !function.is_struct_return {
					statement.children[0].cast_if_reference();
				}
			}
			StatementKind::Expression => {
				self.check_expression(&mut statement.children[0], function, scope)?;
			}
			StatementKind::While(ref mut inner_scope) => {
				// parent should be None since parser doesnt set it
				Rc::get_mut(inner_scope).unwrap().parent = Some(Rc::clone(&scope));

				let ty = self.check_expression(&mut statement.children[0], function, scope)?;
				if ty != BUILTIN_TYPE_BOOL {
					return Err(TypeCheckerError::TypeMismatch(format!(
						"expected bool, got {}",
						self.format_type(ty)
					)));
				}
				statement.children[0].cast_if_reference();
				self.check_scope(Rc::clone(inner_scope), function)?;
			}
			StatementKind::If(ref mut inner_scope) => {
				Rc::get_mut(inner_scope).unwrap().parent = Some(Rc::clone(&scope));

				let ty =
					self.check_expression(&mut statement.children[0], function, Rc::clone(&scope))?;
				if ty != BUILTIN_TYPE_BOOL {
					return Err(TypeCheckerError::TypeMismatch(format!(
						"expected bool, got {:?}",
						ty
					)));
				}
				statement.children[0].cast_if_reference();
				self.check_scope(Rc::clone(inner_scope), function)?;
				if let Some(else_branch) = &mut statement.else_branch {
					self.check_statement(else_branch.borrow_mut(), function, Rc::clone(&scope))?;
				}
			}
			StatementKind::Block(ref mut inner_scope) => {
				Rc::get_mut(inner_scope).unwrap().parent = Some(Rc::clone(&scope));

				self.check_scope(Rc::clone(inner_scope), function)?;
			}
			#[allow(unreachable_patterns)]
			ref k => {
				todo!("{:?}", k);
			}
		}
		Ok(())
	}

	// TODO: maybe not copy the output? idk
	fn find_variable(name: &String, scope: Rc<Scope>, function: &Function) -> Option<Variable> {
		let vars = scope.variables.borrow();
		if let Some(var) = vars.iter().find(|var| &var.name == name) {
			return Some(var.clone());
		}
		if let Some(parent) = &scope.parent {
			let var = Self::find_variable(name, Rc::clone(parent), function);
			if var.is_some() {
				return var;
			}
		}
		// TODO: have functions args be its own scope? maybe
		function
			.arguments
			.iter()
			.find(|var| &var.name == name)
			.cloned()
	}

	fn promote_int_literal_into(&self, expression: &mut Expression, type_ref: TypeRef) -> TypeRef {
		if expression.value_type != BUILTIN_TYPE_INT_LITERAL {
			return expression.value_type;
		}
		let target_type = self.ast.get_type(type_ref);
		if let Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8 | BuiltInType::UPtr) =
			target_type.as_ref()
		{
			expression.value_type = type_ref.remove_reference();
			if let ExpressionKind::Operator(_) = expression.kind {
				for child in &mut expression.children {
					self.promote_int_literal_into(child, type_ref);
				}
			}
		}
		expression.value_type
	}

	fn check_expression(
		&mut self,
		expression: &mut Expression,
		function: &Function,
		scope: Rc<Scope>,
	) -> Result<TypeRef, TypeCheckerError> {
		expression.value_type = self.check_expression_inner(expression, function, scope)?;
		Ok(expression.value_type)
	}

	fn check_expression_inner(
		&mut self,
		expression: &mut Expression,
		function: &Function,
		scope: Rc<Scope>,
	) -> Result<TypeRef, TypeCheckerError> {
		match &expression.kind {
			ExpressionKind::NumberLiteral(_) => Ok(BUILTIN_TYPE_INT_LITERAL),
			ExpressionKind::BoolLiteral(_) => Ok(BUILTIN_TYPE_BOOL),
			ExpressionKind::Operator(Operator::Dot) => {
				let lhs = self.check_expression(
					&mut expression.children[0],
					function,
					Rc::clone(&scope),
				)?;

				let ty = self.ast.get_type(lhs);

				let name =
					if let ExpressionKind::Variable(ref var_name) = &expression.children[1].kind {
						var_name.clone()
					} else {
						return Err(TypeCheckerError::TypeMismatch(format!(
							"Expected identifier, got {:?}",
							expression.children[1].kind
						)));
					};

				let mut struct_type_ref: Option<TypeRef> = None;
				let mut is_pointer = false;
				match ty.as_ref() {
					Type::Struct(_) => {
						struct_type_ref = Some(lhs);
					}
					Type::Pointer(inner) => {
						let ty = self.ast.get_type(*inner);
						// TODO: support member access of struct** ?
						// seems kinda silly
						if let Type::Struct(_) = ty.as_ref() {
							struct_type_ref = Some(*inner);
							is_pointer = true;
						}
					}
					_ => {}
				};

				if let Some(ty) = struct_type_ref {
					expression.kind = ExpressionKind::StructAccess(ty, name);
				} else {
					return Err(TypeCheckerError::TypeMismatch(
						"Expected lhs to be a struct".into(),
					));
				}

				// nuke children
				let first_child;
				{
					let mut drain = expression.children.drain(..);
					first_child = drain.next().unwrap();
				}

				if is_pointer {
					let mut deref = Expression::new(
						ExpressionKind::Operator(Operator::Dereference),
						vec![first_child],
					);
					deref.children[0].cast_if_reference();
					deref.value_type = struct_type_ref.unwrap().add_reference();
					expression.children.push(deref);
				} else {
					expression.children.push(first_child);
				}

				self.check_expression(expression, function, Rc::clone(&scope))
			}
			ExpressionKind::StructAccess(type_ref, field_name) => {
				if let Type::Struct(stru) = self.ast.get_type(*type_ref).as_ref() {
					if let Some(field) = stru.fields.iter().find(|f| &f.name == field_name) {
						Ok(field.ty.add_reference())
					} else {
						Err(TypeCheckerError::TypeMismatch(format!(
							"could not find {} in {}",
							field_name, stru.name
						)))
					}
				} else {
					unreachable!();
				}
			}
			ExpressionKind::ParsedCast(parsed_type) => {
				let inner = self.check_expression(
					&mut expression.children[0],
					function,
					Rc::clone(&scope),
				)?;

				let cast_type_ref = self.check_parsed_type(parsed_type)?;

				let inner_type = self.ast.get_type(inner);
				let cast_type = self.ast.get_type(cast_type_ref);

				match (inner_type.as_ref(), cast_type.as_ref()) {
					(
						Type::BuiltIn(
							BuiltInType::I32
							| BuiltInType::U8
							| BuiltInType::Bool
							| BuiltInType::UPtr,
						),
						Type::BuiltIn(BuiltInType::U8 | BuiltInType::I32 | BuiltInType::UPtr),
					) => {}
					(
						Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8),
						Type::BuiltIn(BuiltInType::Bool),
					) => {}
					(Type::Pointer(_), Type::BuiltIn(BuiltInType::UPtr)) => {}
					(Type::Pointer(_), Type::Pointer(_)) => {}
					(Type::BuiltIn(BuiltInType::UPtr), Type::Pointer(_)) => {}
					(
						Type::BuiltIn(BuiltInType::IntLiteral),
						Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8 | BuiltInType::UPtr),
					) => {
						let mut child = expression.children.pop().unwrap();
						child.value_type = cast_type_ref;
						expression.replace_with(child);
						return Ok(cast_type_ref);
					}
					_ => {
						return Err(TypeCheckerError::InvalidCast);
					}
				}

				expression.children[0].cast_if_reference();

				expression.kind = ExpressionKind::Cast;

				Ok(cast_type_ref)
			}
			ExpressionKind::Operator(op) if op.is_binary() => {
				let lhs;
				if op == &Operator::Assign {
					self.check_expression(
						&mut expression.children[1],
						function,
						Rc::clone(&scope),
					)?;
					lhs = self.check_expression(&mut expression.children[0], function, scope)?;

					if !lhs.reference {
						return Err(TypeCheckerError::TypeMismatch(
							"Left hand side of assignment must be reference".into(),
						));
					}
				} else {
					lhs = self.check_expression(
						&mut expression.children[0],
						function,
						Rc::clone(&scope),
					)?;
					self.check_expression(&mut expression.children[1], function, scope)?;
				}

				let rhs = self.promote_int_literal_into(&mut expression.children[1], lhs);
				let lhs = self.promote_int_literal_into(&mut expression.children[0], rhs);

				if matches!(op, Operator::And | Operator::Or) {
					if lhs != BUILTIN_TYPE_BOOL {
						return Err(TypeCheckerError::TypeMismatch("lhs must be bool".into()));
					}
					if rhs != BUILTIN_TYPE_BOOL {
						return Err(TypeCheckerError::TypeMismatch("rhs must be bool".into()));
					}
				}

				if self.ast.is_pointer(lhs) && !matches!(op, Operator::Assign) {
					let rhs = self
						.promote_int_literal_into(&mut expression.children[1], BUILTIN_TYPE_I32);
					if !matches!(op, Operator::Add | Operator::Sub) || rhs != BUILTIN_TYPE_I32 {
						return Err(TypeCheckerError::TypeMismatch(
							"pointers only support addition and subtraction with i32".into(),
						));
					}
					expression.children[0].cast_if_reference();
					expression.children[1].cast_if_reference();
					return Ok(lhs.remove_reference());
				}

				if lhs != rhs {
					let lhs = self.ast.get_type(lhs);
					let rhs = self.ast.get_type(rhs);
					return Err(TypeCheckerError::TypeMismatch(format!(
						"{:?} and {:?} don't match",
						lhs.as_ref(),
						rhs.as_ref()
					)));
				}

				if matches!(
					op,
					Operator::Add | Operator::Sub | Operator::Divide | Operator::Multiply
				) {
					match self.ast.get_type(lhs).as_ref() {
						Type::BuiltIn(
							BuiltInType::I32
							| BuiltInType::U8
							| BuiltInType::UPtr
							| BuiltInType::IntLiteral,
						) => {}
						_ => {
							return Err(TypeCheckerError::TypeMismatch(format!(
								"arithmetic on {}",
								self.format_type(lhs)
							)));
						}
					}
				}

				if op != &Operator::Assign {
					expression.children[0].cast_if_reference();
				}

				// to fix rhs on struct assignment being reference, when it should be kept for compiler
				if op != &Operator::Assign
					|| !self
						.ast
						.is_struct_or_array(expression.children[1].value_type)
				{
					expression.children[1].cast_if_reference();
				}

				match op {
					Operator::Equals
					| Operator::NotEquals
					| Operator::GreaterThan
					| Operator::GreaterThanEq
					| Operator::LessThan
					| Operator::LessThanEq => Ok(BUILTIN_TYPE_BOOL),
					Operator::Assign => Ok(BUILTIN_TYPE_VOID),
					_ => Ok(lhs.remove_reference()),
				}
			}
			ExpressionKind::Operator(Operator::Negate) => {
				self.check_expression(&mut expression.children[0], function, Rc::clone(&scope))?;

				let ty =
					self.promote_int_literal_into(&mut expression.children[0], BUILTIN_TYPE_I32);

				if ty != BUILTIN_TYPE_I32 {
					return Err(TypeCheckerError::TypeMismatch(format!(
						"negation only works on i32, got {:?}",
						ty
					)));
				}

				expression.children[0].cast_if_reference();

				Ok(ty.remove_reference())
			}
			ExpressionKind::Declaration(var) => {
				scope.variables.borrow_mut().push(var.clone());
				Ok(var.ty.add_reference())
			}
			ExpressionKind::Variable(name) => {
				if let Some(var) = Self::find_variable(name, scope, function) {
					Ok(var.ty.add_reference())
				} else {
					Err(TypeCheckerError::VariableNotFound(name.clone()))
				}
			}
			ExpressionKind::Call(name) => {
				if let Some(func) = self
					.parser
					.functions
					.iter()
					.find(|&f| &f.borrow().name == name)
				{
					let func = func.borrow();
					if func.arguments.len() != expression.children.len() {
						return Err(TypeCheckerError::ArgumentCountMismatch);
					}
					for (arg, exp) in func.arguments.iter().zip(expression.children.iter_mut()) {
						self.check_expression(exp, function, Rc::clone(&scope))?;
						if !self.ast.is_struct_or_array(exp.value_type) {
							exp.cast_if_reference();
						}

						let ty = self.promote_int_literal_into(exp, arg.ty);

						if ty != arg.ty {
							return Err(TypeCheckerError::TypeMismatch(format!(
								"{} did not match {}",
								self.format_type(ty),
								self.format_type(arg.ty)
							)));
						}
					}
					if func.is_struct_return {
						function.scope.variables.borrow_mut().push(Variable {
							name: "$struct_space".into(),
							ty: func.return_type,
						});
						Ok(func.return_type.add_reference())
					} else {
						Ok(func.return_type)
					}
				} else if name == "syscall" {
					if expression.children.len() > 7 || expression.children.is_empty() {
						return Err(TypeCheckerError::ArgumentCountMismatch);
					}
					for exp in expression.children.iter_mut() {
						self.check_expression(exp, function, Rc::clone(&scope))?;
						exp.cast_if_reference();

						self.promote_int_literal_into(exp, BUILTIN_TYPE_I32);

						// TODO: check if types arent like structs or something
					}
					Ok(BUILTIN_TYPE_I32)
				} else {
					Err(TypeCheckerError::FunctionNotFound(name.clone()))
				}
			}
			ExpressionKind::Operator(Operator::Reference) => {
				let type_ref =
					self.check_expression(&mut expression.children[0], function, scope)?;
				if !type_ref.reference {
					Err(TypeCheckerError::InvalidReference)
				} else {
					Ok(self
						.ast
						.find_type_or_add(Type::Pointer(type_ref.remove_reference())))
				}
			}
			ExpressionKind::Operator(Operator::Dereference) => {
				let type_ref =
					self.check_expression(&mut expression.children[0], function, scope)?;
				match self.ast.get_type(type_ref).as_ref() {
					Type::Pointer(inner) => {
						expression.children[0].cast_if_reference();
						Ok(inner.add_reference())
					}
					_ => Err(TypeCheckerError::InvalidDereference),
				}
			}
			ExpressionKind::ParsedDeclaration(parsed_var) => {
				let new = Expression::new(
					ExpressionKind::Declaration(self.check_parsed_var(parsed_var)?),
					vec![],
				);
				expression.replace_with(new);
				self.check_expression(expression, function, scope)
			}
			ExpressionKind::AsmLiteral(_) => Ok(BUILTIN_TYPE_VOID),
			ExpressionKind::StringLiteral(_) => {
				// create a fake variable just so compiler creates enough space in the scope..
				scope.variables.borrow_mut().push(Variable {
					name: "$str".into(),
					ty: BUILTIN_TYPE_STR,
				});
				// TODO: this is kinda hacky, it matches what compiler was doing anyways but
				// should probably not be intented behavior
				Ok(BUILTIN_TYPE_STR.add_reference())
			}
			ExpressionKind::ArrayLiteral => {
				let mut guessed_type = TypeRef::unknown();
				for exp in expression.children.iter_mut() {
					self.check_expression(exp, function, Rc::clone(&scope))?;
					// check if its struct..
					exp.cast_if_reference();

					let ty = exp.value_type;
					if ty != BUILTIN_TYPE_INT_LITERAL {
						if guessed_type.is_unknown() {
							guessed_type = ty;
						} else if ty != guessed_type {
							return Err(TypeCheckerError::TypeMismatch(format!(
								"expected {}, got {}",
								self.format_type(guessed_type),
								self.format_type(ty)
							)));
						}
					}
				}

				if guessed_type.is_unknown() && !expression.children.is_empty() {
					guessed_type = BUILTIN_TYPE_INT_LITERAL;
				} else {
					for exp in expression.children.iter_mut() {
						self.promote_int_literal_into(exp, guessed_type);
					}
				}
				// copy StringLiteral behavior, workaround while the compiler is really dumb
				// and cant cast struct& to struct
				Ok(self
					.ast
					.find_type_or_add(Type::Array(guessed_type, expression.children.len()))
					.add_reference())
			}
			ExpressionKind::ArrayIndex => {
				let array_like = self.check_expression(
					&mut expression.children[0],
					function,
					Rc::clone(&scope),
				)?;
				if self.ast.is_pointer(array_like) {
					expression.children[0].cast_if_reference();
				}

				self.check_expression(&mut expression.children[1], function, scope)?;
				expression.children[1].cast_if_reference();
				let index_type =
					self.promote_int_literal_into(&mut expression.children[1], BUILTIN_TYPE_I32);

				// FIXME: i really need better errors

				if !self.ast.is_integer(index_type) {
					return Err(TypeCheckerError::TypeMismatch(
						"expected integer or smth".into(),
					));
				}

				let ty = self.ast.get_type(array_like);
				if let Type::Pointer(inner) = ty.as_ref() {
					Ok(inner.add_reference())
				} else if let Type::Array(inner, _) = ty.as_ref() {
					Ok(inner.add_reference())
				} else {
					Err(TypeCheckerError::TypeMismatch(
						"expected array or pointer".into(),
					))
				}
			}
			k => {
				todo!("{:?}", k)
			}
		}
	}

	fn check_parsed_type(&mut self, parsed_type: &ParsedType) -> Result<TypeRef, TypeCheckerError> {
		match parsed_type {
			ParsedType::Name(name) => self
				.ast
				.find_type_by_name(name)
				.ok_or_else(|| TypeCheckerError::UnknownType(name.clone())),
			ParsedType::Pointer(inner) => {
				let inner = self.check_parsed_type(inner)?;
				Ok(self.ast.find_type_or_add(Type::Pointer(inner)))
			}
			ParsedType::Array(inner, size) => {
				let inner = self.check_parsed_type(inner)?;
				Ok(self.ast.find_type_or_add(Type::Array(inner, *size)))
			}
			ParsedType::Unknown => unreachable!(),
		}
	}
}
