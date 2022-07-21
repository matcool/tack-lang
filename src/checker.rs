use crate::{
	lexer::Operator,
	parser::{Expression, ExpressionKind, Function, Parser, Scope, Statement, StatementKind, Type, Variable},
};

impl Type {
	fn unref_eq(&self, other: &Type) -> bool {
		self.name == other.name
	}

	fn add_reference(&self) -> Self {
		let mut c = self.clone();
		c.reference = true;
		c
	}

	fn remove_reference(&self) -> Self {
		let mut c = self.clone();
		c.reference = false;
		c
	}
}

impl Expression {
	fn replace_with_cast(&mut self, typ: Type) {
		let mut cast = Expression::new(ExpressionKind::Cast, vec![]);
		cast.value_type = typ;
		std::mem::swap(self, &mut cast);
		// self and cast have swapped
		// so this would actually be cast.children.push(self)
		self.children.push(cast);
	}
}

#[derive(Debug)]
pub enum TypeCheckerError {
	TypeMismatch(String),
	VariableNotFound(String),
	VariableAlreadyExists(String),
	FunctionNotFound(String),
	ArgumentCountMismatch, // great name
}

pub struct TypeChecker<'a> {
	parser: &'a Parser,
}

impl TypeChecker<'_> {
	pub fn new(parser: &'_ Parser) -> TypeChecker {
		TypeChecker { parser }
	}

	pub fn check(&self) -> Result<(), TypeCheckerError> {
		for function in &self.parser.functions {
			self.check_function(function)?;
		}
		Ok(())
	}

	fn check_function(&self, function: &Function) -> Result<(), TypeCheckerError> {
		for statement in &function.scope.statements {
			self.check_statement(&mut statement.borrow_mut(), function, &function.scope)?;
		}
		Ok(())
	}

	fn check_statement(
		&self,
		statement: &mut Statement,
		function: &Function,
		scope: &Scope,
	) -> Result<(), TypeCheckerError> {
		match statement.kind {
			StatementKind::Return => {
				let ty = self.check_expression(&mut statement.children[0], function, scope)?;
				if !ty.unref_eq(&function.return_type) {
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
		}
		Ok(())
	}

	// TODO: maybe not copy the output? idk
	fn find_variable(name: &String, scope: &Scope, function: &Function) -> Option<Variable> {
		let vars = scope.variables.borrow();
		if let Some(var) = vars.iter().find(|var| &var.name == name) {
			return Some(var.clone());
		}
		if let Some(scope) = &scope.parent {
			let var = Self::find_variable(name, scope, function);
			if var.is_some() { return var; }
		}
		function.arguments.iter().find(|var| &var.name == name).map(|x| x.clone())
	}

	fn check_expression(
		&self,
		expression: &mut Expression,
		function: &Function,
		scope: &Scope,
	) -> Result<Type, TypeCheckerError> {
		expression.value_type = self.check_expression_inner(expression, function, scope)?;
		// TODO: return a borrow?
		// or maybe have some global list of types..
		// wouldnt want to be copying struct types everywhere
		Ok(expression.value_type.clone())
	}

	fn check_expression_inner(
		&self,
		expression: &mut Expression,
		function: &Function,
		scope: &Scope,
	) -> Result<Type, TypeCheckerError> {
		match &expression.kind {
			ExpressionKind::NumberLiteral(_) => Ok(Type::new("i32")),
			ExpressionKind::BoolLiteral(_) => Ok(Type::new("bool")),
			ExpressionKind::Operator(op) if op.is_binary() => {
				let lhs;
				let rhs;
				if op == &Operator::Assign {
					rhs = self.check_expression(&mut expression.children[1], function, scope)?;
					lhs = self.check_expression(&mut expression.children[0], function, scope)?;

					if !lhs.reference {
						return Err(TypeCheckerError::TypeMismatch(
							"Left hand side of assignment must be reference".into(),
						));
					}
				} else {
					lhs = self.check_expression(&mut expression.children[0], function, scope)?;
					rhs = self.check_expression(&mut expression.children[1], function, scope)?;
				}

				if !lhs.unref_eq(&rhs) {
					return Err(TypeCheckerError::TypeMismatch(format!(
						"{:?} and {:?} don't match",
						lhs, rhs
					)));
				}

				if op != &Operator::Assign {
					if lhs.reference {
						expression.children[0].replace_with_cast(lhs.remove_reference());
					}
				}

				if rhs.reference {
					expression.children[1].replace_with_cast(rhs.remove_reference());
				}

				match op {
					Operator::Equals | Operator::NotEquals => Ok(Type::new("bool")),
					_ => Ok(lhs.remove_reference()),
				}
			}
			ExpressionKind::Declaration(var) => {
				// if let Some(var) = Self::find_variable(&var.name, scope, function) {
				// 	Err(TypeCheckerError::VariableAlreadyExists(var.name.clone()))
				// } else {
					scope.variables.borrow_mut().push(var.clone());
					Ok(var.ty.add_reference())
				// }
			}
			ExpressionKind::Variable(name) => {
				if let Some(var) = Self::find_variable(name, scope, function) {
					Ok(var.ty.add_reference())
				} else {
					Err(TypeCheckerError::VariableNotFound(name.clone()))
				}
			}
			ExpressionKind::Call(name) => {
				if let Some(func) = self.parser.functions.iter().find(|f| &f.name == name) {
					if func.arguments.len() != expression.children.len() {
						return Err(TypeCheckerError::ArgumentCountMismatch);
					}
					for (arg, exp) in func.arguments.iter().zip(expression.children.iter_mut()) {
						let ty = self.check_expression(exp, function, scope)?;
						if ty.reference {
							exp.replace_with_cast(ty.remove_reference());
						}

						if !ty.unref_eq(&arg.ty) {
							return Err(TypeCheckerError::TypeMismatch("its wrong buddy".into()));
						}
					}
					Ok(func.return_type.clone())
				} else {
					Err(TypeCheckerError::FunctionNotFound(name.clone()))
				}
			}
			k => {
				todo!("{:?}", k)
			}
		}
	}
}
