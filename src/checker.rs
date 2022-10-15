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
	pub fn name(&self) -> Option<String> {
		match self {
			Type::BuiltIn(builtin) => Some(match builtin {
				BuiltInType::I32 => "i32".to_string(),
				BuiltInType::Bool => "bool".to_string(),
			}),
			Type::Pointer(_) => None,
			Type::Struct(str) => Some(str.name.clone()),
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
		self.find_type(|&t| t.name().map_or(false, |s| &s == name))
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
}

impl Expression {
	fn replace_with(&mut self, mut new: Expression) {
		std::mem::swap(self, &mut new);
	}

	fn replace_with_cast(&mut self, typ: TypeRef) {
		let mut cast = Expression::new(ExpressionKind::Cast, vec![]);
		cast.value_type = typ;
		std::mem::swap(self, &mut cast);
		// self and new have swapped
		// so this would actually be cast.children.push(self)
		self.children.push(cast);
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
}

const BUILTIN_TYPE_I32: TypeRef = TypeRef::new(0);
const BUILTIN_TYPE_BOOL: TypeRef = TypeRef::new(1);

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
		self.ast.add_type(Type::BuiltIn(BuiltInType::Bool));

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
		for statement in &scope.statements {
			self.check_statement(&mut statement.borrow_mut(), function, Rc::clone(&scope))?;
		}
		Ok(())
	}

	fn check_statement(
		&mut self,
		statement: &mut Statement,
		function: &Function,
		scope: Rc<Scope>,
	) -> Result<(), TypeCheckerError> {
		match statement.kind {
			StatementKind::Return => {
				let ty = self.check_expression(&mut statement.children[0], function, scope)?;
				if ty != function.return_type {
					return Err(TypeCheckerError::TypeMismatch(
						"between return type and the expression".into(),
					));
				}
				if ty.reference && !function.return_type.reference {
					statement.children[0].replace_with_cast(ty.remove_reference());
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
						"expected bool, got {:?}",
						ty
					)));
				}
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
				self.check_scope(Rc::clone(inner_scope), function)?;
				if let Some(else_branch) = &mut statement.else_branch {
					self.check_statement(else_branch.borrow_mut(), function, Rc::clone(&scope))?;
				}
			}
			StatementKind::Block(ref mut inner_scope) => {
				Rc::get_mut(inner_scope).unwrap().parent = Some(Rc::clone(&scope));

				self.check_scope(Rc::clone(inner_scope), function)?;
			}
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
			ExpressionKind::NumberLiteral(_) => Ok(BUILTIN_TYPE_I32),
			ExpressionKind::BoolLiteral(_) => Ok(BUILTIN_TYPE_BOOL),
			ExpressionKind::Operator(op) if op.is_binary() => {
				let lhs;
				let rhs;
				if op == &Operator::Assign {
					rhs = self.check_expression(
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
				} else if op == &Operator::Dot {
					lhs = self.check_expression(&mut expression.children[0], function, scope)?;

					let ty = self.ast.get_type(lhs);

					if let Type::Struct(stru) = ty.as_ref() {
						#[rustfmt::skip]
						let name = if let ExpressionKind::Variable(ref var_name) = &expression.children[1].kind {
							var_name.clone()
						} else {
							return Err(TypeCheckerError::TypeMismatch(
								"You stink at member access".into(),
							));
						};
						if let Some(field) = stru.fields.iter().find(|f| f.name == name) {
							expression.children[1].value_type = field.ty.add_reference();
							return Ok(field.ty.add_reference());
						} else {
							println!("could not find {} in {}", name, stru.name);
							return Err(TypeCheckerError::TypeMismatch(
								"You stink at member access again".into(),
							));
						}
					} else {
						return Err(TypeCheckerError::TypeMismatch(
							"Expected lhs to be a struct".into(),
						));
					}
				} else {
					lhs = self.check_expression(
						&mut expression.children[0],
						function,
						Rc::clone(&scope),
					)?;
					rhs = self.check_expression(&mut expression.children[1], function, scope)?;
				}

				if lhs != rhs {
					return Err(TypeCheckerError::TypeMismatch(format!(
						"{:?} and {:?} don't match",
						lhs, rhs
					)));
				}

				if op != &Operator::Assign && lhs.reference {
					expression.children[0].replace_with_cast(lhs.remove_reference());
				}

				if rhs.reference {
					expression.children[1].replace_with_cast(rhs.remove_reference());
				}

				match op {
					Operator::Equals | Operator::NotEquals => Ok(BUILTIN_TYPE_BOOL),
					_ => Ok(lhs.remove_reference()),
				}
			}
			ExpressionKind::Operator(Operator::Negate) => {
				let ty = self.check_expression(
					&mut expression.children[0],
					function,
					Rc::clone(&scope),
				)?;

				if ty != BUILTIN_TYPE_I32 {
					return Err(TypeCheckerError::TypeMismatch(format!(
						"negation only works on i32, got {:?}",
						ty
					)));
				}

				if ty.reference {
					expression.children[0].replace_with_cast(ty.remove_reference());
				}

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
						let ty = self.check_expression(exp, function, Rc::clone(&scope))?;
						if ty.reference {
							exp.replace_with_cast(ty.remove_reference());
						}

						if ty != arg.ty {
							return Err(TypeCheckerError::TypeMismatch("its wrong buddy".into()));
						}
					}
					Ok(func.return_type)
				} else if let Some(type_ref) = self.ast.find_type_by_name(name) {
					let ty = self.ast.get_type(type_ref);
					if let Type::Struct(stru) = ty.as_ref() {
						if stru.fields.len() != expression.children.len() {
							return Err(TypeCheckerError::ArgumentCountMismatch);
						}
						for (field, exp) in stru.fields.iter().zip(expression.children.iter_mut()) {
							let ty = self.check_expression(exp, function, Rc::clone(&scope))?;
							if ty.reference {
								exp.replace_with_cast(ty.remove_reference());
							}

							if ty != field.ty {
								return Err(TypeCheckerError::TypeMismatch(
									"its wrong buddy".into(),
								));
							}
						}
						Ok(type_ref)
					} else {
						Err(TypeCheckerError::FunctionNotFound(name.clone()))
					}
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
					Type::Pointer(inner) => Ok(inner.add_reference()),
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
			ParsedType::Unknown => unreachable!(),
		}
	}
}
