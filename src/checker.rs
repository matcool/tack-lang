use std::cell::{Ref, RefCell};

use crate::parser::{Function, Parser, Statement, StatementKind, Expression, Type};

pub struct TypeChecker<'a> {
	parser: std::cell::RefCell<&'a mut Parser>,
}

#[derive(Debug)]
pub enum TypeCheckerError {
	TypeMismatch(String)
}

impl TypeChecker<'_> {
	pub fn new(parser: &mut Parser) -> TypeChecker {
		TypeChecker {
			parser: parser.into(),
		}
	}

	pub fn check(&mut self) -> Result<(), TypeCheckerError> {
		for function in &mut self.parser.borrow_mut().functions {
			self.check_function(function)?;
		}
		Ok(())
	}

	fn check_function(&self, function: &mut Function) -> Result<(), TypeCheckerError> {
		for statement in &mut function.statements {
			self.check_statement(statement, function)?;
		}
		Ok(())
	}

	fn check_statement(
		&self,
		statement: &mut Statement,
		function: &Function,
	) -> Result<(), TypeCheckerError> {
		match statement.kind {
			StatementKind::Return => {
				let ty = self.check_expression(&mut statement.children[0], function)?;
				if ty.name != function.return_type.name {
					return Err(TypeCheckerError::TypeMismatch("between return type and the expression".into()));
				}
			}
			StatementKind::Expression => {}
		}
		Ok(())
	}

	fn check_expression(&self, expression: &mut Expression, function: &Function) -> Result<Type, TypeCheckerError> {
		Err(TypeCheckerError::TypeMismatch("poop".into()))
	}
}
