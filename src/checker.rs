use std::borrow::Borrow;

use crate::{
	lexer::Operator,
	parser::{Expression, ExpressionKind, Function, Parser, Scope, Statement, StatementKind, Type},
};

impl Type {
	fn unref_eq(&self, other: &Type) -> bool {
		self.name == other.name
	}
}

#[derive(Debug)]
pub enum TypeCheckerError {
	TypeMismatch(String),
	VariableNotFound(String),
}

pub fn check_types(parser: &Parser) -> Result<(), TypeCheckerError> {
	for function in &parser.functions {
		check_function(parser, function)?;
	}
	Ok(())
}

fn check_function(parser: &Parser, function: &Function) -> Result<(), TypeCheckerError> {
	for statement in &function.scope.statements {
		check_statement(
			parser,
			&mut statement.borrow_mut(),
			function,
			&function.scope,
		)?;
	}
	Ok(())
}

fn check_statement(
	parser: &Parser,
	statement: &mut Statement,
	function: &Function,
	scope: &Scope,
) -> Result<(), TypeCheckerError> {
	match statement.kind {
		StatementKind::Return => {
			let ty = check_expression(parser, &mut statement.children[0], function, scope)?;
			if ty.name != function.return_type.name {
				return Err(TypeCheckerError::TypeMismatch(
					"between return type and the expression".into(),
				));
			}
		}
		StatementKind::Expression => {
			check_expression(parser, &mut statement.children[0], function, scope)?;
		}
	}
	Ok(())
}

fn check_expression(
	parser: &Parser,
	expression: &mut Expression,
	function: &Function,
	scope: &Scope,
) -> Result<Type, TypeCheckerError> {
	match &expression.kind {
		ExpressionKind::NumberLiteral(_) => Ok(Type { name: "i32".into() }),
		ExpressionKind::BoolLiteral(_) => Ok(Type {
			name: "bool".into(),
		}),
		ExpressionKind::Operator(op) if op.is_binary() => {
			let rhs = check_expression(parser, &mut expression.children[1], function, scope)?;
			let lhs = check_expression(parser, &mut expression.children[0], function, scope)?;

			if !lhs.unref_eq(&rhs) {
				return Err(TypeCheckerError::TypeMismatch(format!(
					"{:?} and {:?} don't match",
					lhs, rhs
				)));
			}

			match op {
				Operator::Equals | Operator::NotEquals => Ok(Type {
					name: "bool".into(),
				}),
				_ => Ok(Type { name: lhs.name }),
			}
		}
		ExpressionKind::Declaration(var) => {
			scope.variables.borrow_mut().push(var.clone());
			Ok(var.ty.clone())
		}
		ExpressionKind::Variable(name) => {
			Ok(
				scope
				.variables
				.borrow()
				.iter()
				.find(|var| &var.name == name)
				.ok_or(TypeCheckerError::VariableNotFound(name.clone()))?
				.ty.clone()
			)
		}
		k => {
			todo!("fuck {:?}", k)
		}
	}
}
