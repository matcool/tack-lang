use std::{path::PathBuf, rc::Rc};

use crate::{
	ast::{
		BuiltInType, Expression, ExpressionKind, Function, Scope, Statement, StatementKind,
		StructType, Type, TypeRef, Variable, AST, BUILTIN_TYPE_BOOL, BUILTIN_TYPE_I32,
		BUILTIN_TYPE_INT_LITERAL, BUILTIN_TYPE_VOID,
	},
	lexer::{Operator, Span},
	parser::{self, Parser},
};

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

pub struct TypeChecker {
	pub ast: AST,
	pub file_path: PathBuf,
}

struct FunctionTypeChecker<'a> {
	ast: &'a mut AST,
	function: &'a Function,
	scope: Rc<Scope>,
}

impl TypeChecker {
	pub fn new(file_path: PathBuf) -> TypeChecker {
		TypeChecker {
			ast: AST::new(file_path.clone()),
			file_path,
		}
	}

	pub fn check(mut self, parser: Parser) -> Result<Vec<AST>, TypeCheckerError> {
		let mut asts = Vec::new();

		for parsed_struct in parser.parsed_structs {
			let mut fields: Vec<Variable> = vec![];
			for field in parsed_struct.fields {
				if fields.iter().any(|f| f.name == field.name) {
					return Err(TypeCheckerError::FieldAlreadyExists(field.name.clone()));
				}
				fields.push(self.ast.check_parsed_var(field)?);
			}
			self.ast.add_type(Type::Struct(StructType {
				name: parsed_struct.name.clone(),
				fields,
			}));
		}

		for function in parser.functions {
			let function = self.check_function(function)?;
			self.ast.functions.push(function);
		}

		asts.insert(0, self.ast);
		Ok(asts)
	}

	fn check_function(&mut self, parsed: parser::Function) -> Result<Function, TypeCheckerError> {
		let return_type = self.ast.check_parsed_type(parsed.return_type)?;
		let mut arguments = vec![];
		for arg in parsed.arguments {
			arguments.push(self.ast.check_parsed_var(arg)?);
		}

		let mut function = Function::new(parsed.name);
		function.return_type = return_type;
		function.arguments = arguments;
		let scope = self.check_scope(parsed.scope, &function)?;
		function.scope = scope.into();

		Ok(function)
	}

	fn check_scope(
		&mut self,
		parsed: parser::Scope,
		function: &Function,
	) -> Result<Rc<Scope>, TypeCheckerError> {
		let scope = Rc::new(Scope::new(None));
		let mut checker = FunctionTypeChecker {
			ast: &mut self.ast,
			function,
			scope: Rc::clone(&scope),
		};
		for statement in parsed.statements {
			let stmt = checker.check_statement(statement)?;
			scope.add_statement(stmt);
		}
		Ok(scope)
	}
}

impl AST {
	fn check_parsed_type(&mut self, parsed: parser::Type) -> Result<TypeRef, TypeCheckerError> {
		match parsed {
			parser::Type::Name(name) => self
				.find_type_by_name(&name)
				.ok_or_else(|| TypeCheckerError::UnknownType(name.clone())),
			parser::Type::Pointer(inner) => {
				let inner = self.check_parsed_type(*inner)?;
				Ok(self.find_type_or_add(Type::Pointer(inner)))
			}
			parser::Type::Array(inner, size) => {
				let inner = self.check_parsed_type(*inner)?;
				Ok(self.find_type_or_add(Type::Array(inner, size)))
			}
			parser::Type::Unknown => unreachable!(),
		}
	}

	fn check_parsed_var(&mut self, parsed: parser::Variable) -> Result<Variable, TypeCheckerError> {
		Ok(Variable {
			name: parsed.name.clone(),
			unique_id: 0,
			ty: self.check_parsed_type(parsed.ty)?,
		})
	}
}

impl FunctionTypeChecker<'_> {
	fn check_scope(&mut self, parsed: parser::Scope) -> Result<Rc<Scope>, TypeCheckerError> {
		let scope = Rc::new(Scope::new(Some(Rc::clone(&self.scope))));
		let mut checker = FunctionTypeChecker {
			ast: self.ast,
			function: self.function,
			scope: Rc::clone(&scope),
		};
		for statement in parsed.statements {
			let stmt = checker.check_statement(statement)?;
			scope.add_statement(stmt);
		}
		Ok(scope)
	}

	fn check_statement(
		&mut self,
		mut parsed: parser::Statement,
	) -> Result<Statement, TypeCheckerError> {
		Ok(match parsed.kind {
			parser::StatementKind::Expression => {
				let expr = self.check_expression(parsed.children.remove(0))?;
				Statement::new(StatementKind::Expression, vec![expr])
			}
			parser::StatementKind::Return => {
				let mut expr = self.check_expression(parsed.children.remove(0))?;
				let ty = self.promote_int_literal_into(&mut expr, self.function.return_type);
				if ty != self.function.return_type {
					return Err(TypeCheckerError::TypeMismatch(
						"between return type and the expression".into(),
					));
				}
				if !self.function.return_type.reference && !self.function.is_struct_return {
					expr.cast_if_reference();
				}
				Statement::new(StatementKind::Return, vec![expr])
			}
			parser::StatementKind::If(parsed_scope, else_stmt) => {
				let mut condition = self.check_expression(parsed.children.remove(0))?;
				condition.cast_if_reference();
				if condition.value_type != BUILTIN_TYPE_BOOL {
					return Err(TypeCheckerError::TypeMismatch(
						"Condition must be a boolean".into(),
					));
				}
				let if_scope = self.check_scope(parsed_scope)?;
				let else_stmt = else_stmt
					.map(|stmt| self.check_statement(*stmt))
					.transpose()?
					.map(Box::new);

				Statement::new(StatementKind::If(if_scope, else_stmt), vec![condition])
			}
			parser::StatementKind::Block(parsed_scope) => {
				let scope = self.check_scope(parsed_scope)?;
				Statement::new(StatementKind::Block(scope), vec![])
			}
			_ => todo!("{:?}", parsed),
		})
	}

	fn check_expression(
		&mut self,
		parsed: parser::Expression,
	) -> Result<Expression, TypeCheckerError> {
		Ok(match parsed.kind {
			parser::ExpressionKind::NumberLiteral(_) => {
				self.check_expression_into(parsed, BUILTIN_TYPE_INT_LITERAL)?
			}
			parser::ExpressionKind::BoolLiteral(_) => {
				self.check_expression_into(parsed, BUILTIN_TYPE_BOOL)?
			}
			parser::ExpressionKind::Operator(Operator::Assign) => {
				let mut expr = self.check_expression_into(parsed, TypeRef::unknown())?;
				let lhs = expr.children[0].value_type;
				if !lhs.reference {
					return Err(TypeCheckerError::TypeMismatch(
						"Left hand side of assigment must be reference".into(),
					));
				}

				// Promote rhs into lhs if possible
				let rhs = self.promote_int_literal_into(&mut expr.children[1], lhs);

				if lhs != rhs {
					return Err(TypeCheckerError::TypeMismatch(format!(
						"{} and {} don't match",
						self.format_type(lhs),
						self.format_type(rhs)
					)));
				}

				expr.value_type = BUILTIN_TYPE_VOID;
				expr
			}
			parser::ExpressionKind::Operator(op) if op.is_binary() => {
				let mut expr = self.check_expression_into(parsed, TypeRef::unknown())?;
				let lhs = expr.children[0].value_type;

				// Promote rhs into lhs if possible
				let rhs = self.promote_int_literal_into(&mut expr.children[1], lhs);
				// Otherwise, promote lhs into rhs
				let lhs = self.promote_int_literal_into(&mut expr.children[0], rhs);

				// Boolean operators
				if matches!(op, Operator::And | Operator::Or) {
					if lhs != BUILTIN_TYPE_BOOL || rhs != BUILTIN_TYPE_BOOL {
						return Err(TypeCheckerError::TypeMismatch(
							"Operands must be bool".into(),
						));
					}
				}

				// Pointer arithmetic
				if self.ast.is_pointer(lhs) {
					let rhs =
						self.promote_int_literal_into(&mut expr.children[1], BUILTIN_TYPE_I32);
					if !matches!(op, Operator::Add | Operator::Sub) || rhs != BUILTIN_TYPE_I32 {
						return Err(TypeCheckerError::TypeMismatch(
							"Pointers only support addition and subtraction with i32".into(),
						));
					}
					expr.children[0].cast_if_reference();
					expr.children[1].cast_if_reference();
					expr.value_type = lhs.remove_reference();
					return Ok(expr);
				}

				if lhs != rhs {
					return Err(TypeCheckerError::TypeMismatch(format!(
						"Operand types {} and {} don't match",
						self.format_type(lhs),
						self.format_type(rhs)
					)));
				}

				if matches!(
					op,
					Operator::Add | Operator::Sub | Operator::Divide | Operator::Multiply
				) {
					let ty = self.ast.get_type(lhs);
					if !matches!(
						ty,
						Type::BuiltIn(
							BuiltInType::I32
								| BuiltInType::UPtr | BuiltInType::U8
								| BuiltInType::IntLiteral
						)
					) {
						return Err(TypeCheckerError::TypeMismatch(format!(
							"Arithmetic on non integer type ({})",
							self.format_type(lhs)
						)));
					}
				}

				expr.children[0].cast_if_reference();
				expr.children[1].cast_if_reference();

				expr.value_type = match op {
					Operator::Equals
					| Operator::NotEquals
					| Operator::GreaterThan
					| Operator::GreaterThanEq
					| Operator::LessThan
					| Operator::LessThanEq => BUILTIN_TYPE_BOOL,
					_ => lhs.remove_reference(),
				};
				expr
			}
			parser::ExpressionKind::Declaration(parsed_var) => {
				let var = self
					.scope
					.add_variable(self.ast.check_parsed_var(parsed_var)?);
				self.check_expression_with(
					var.ty.add_reference(),
					ExpressionKind::Declaration(var),
					parsed.span,
				)
			}
			parser::ExpressionKind::Identifier(ref name) => {
				let Some(var) = self.find_variable(name) else {
					Err(TypeCheckerError::VariableNotFound(name.clone()))?
				};
				self.check_expression_with(
					var.ty.add_reference(),
					ExpressionKind::Identifier(var),
					parsed.span,
				)
			}
			_ => todo!("{:?}", parsed),
		})
	}

	/// Checks an expression into a specified type,
	/// and calls `check_expression` on its children.
	fn check_expression_into(
		&mut self,
		parsed: parser::Expression,
		ty: TypeRef,
	) -> Result<Expression, TypeCheckerError> {
		let children: Result<_, TypeCheckerError> = parsed
			.children
			.into_iter()
			.map(|c| self.check_expression(c))
			.collect();
		let children = children?;
		let mut expr = Expression::new(parsed.kind.try_into().unwrap(), children);
		expr.value_type = ty;
		expr.span = parsed.span;
		Ok(expr)
	}

	fn check_expression_with(
		&mut self,
		ty: TypeRef,
		kind: ExpressionKind,
		span: Span,
	) -> Expression {
		let mut expr = Expression::new(kind, vec![]);
		expr.value_type = ty;
		expr.span = span;
		expr
	}

	fn promote_int_literal_into(&self, expression: &mut Expression, type_ref: TypeRef) -> TypeRef {
		if expression.value_type != BUILTIN_TYPE_INT_LITERAL {
			return expression.value_type;
		}
		let target_type = self.ast.get_type(type_ref);
		if let Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8 | BuiltInType::UPtr) = target_type {
			expression.value_type = type_ref.remove_reference();
			if let ExpressionKind::Operator(_) = expression.kind {
				for child in &mut expression.children {
					self.promote_int_literal_into(child, type_ref);
				}
			}
		}
		expression.value_type
	}

	fn find_variable(&self, name: &str) -> Option<Variable> {
		if let Some(var) = self.scope.find_variable(name) {
			return Some(var);
		} else if let Some(var) = self
			.scope
			.parent
			.clone()
			.and_then(|s| s.find_variable(name))
		{
			return Some(var);
		}
		self.function
			.arguments
			.iter()
			.find(|var| var.name == name)
			.cloned()
	}

	fn format_type(&self, type_ref: TypeRef) -> String {
		type_ref.formatted(self.ast)
	}
}
