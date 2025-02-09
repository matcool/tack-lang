use std::collections::HashMap;

use crate::{
	ast::{
		self, Expression, ExpressionKind, Function, Scope, Statement, StatementKind, TypeRef, AST,
	},
	lexer::Operator,
};

pub struct Compiler<'a> {
	ast: &'a AST,
	declarations: String,
	body: String,
	counter: i32,
	variables: HashMap<String, String>,
}

impl Compiler<'_> {
	pub fn new(ast: &AST) -> Compiler {
		Compiler {
			ast,
			declarations: Default::default(),
			body: Default::default(),
			counter: 0,
			variables: Default::default(),
		}
	}

	pub fn compile(mut self) -> String {
		let mut output = String::new();
		output += &format!("#include <stdbool.h>\n");
		output += &format!("#include <stdint.h>\n");
		output += &format!("typedef int32_t i32;\n");
		output += &format!("typedef uint8_t u8;\n");
		output += &format!("typedef uintptr_t uptr;\n");
		output += "\n";

		for function in &self.ast.functions {
			output += &self.compile_function(function);
		}
		output
	}

	fn reset_values(&mut self) {
		self.declarations = String::new();
		self.body = String::new();
		self.counter = 0;
		self.variables = HashMap::new();
	}

	fn compile_function(&mut self, function: &Function) -> String {
		let mut output = String::new();
		self.reset_values();
		output += &format!(
			"{} {}() {{\n",
			function.return_type.formatted(self.ast),
			function.name
		);

		self.compile_scope(&*function.scope);

		output += &self.declarations;
		output += &self.body;
		output += "}\n";

		output
	}

	fn compile_scope(&mut self, scope: &Scope) {
		for stmt in scope.statements.borrow().iter() {
			self.compile_statement(stmt);
		}
	}

	fn compile_statement(&mut self, stmt: &Statement) {
		match &stmt.kind {
			StatementKind::Return => {
				let value = self.compile_expression(&stmt.children[0]);
				self.body += &format!("return {value};\n");
			}
			StatementKind::Expression => {
				self.compile_expression(&stmt.children[0]);
			}
			StatementKind::If(scope, else_stmt) => {
				let cond = self.compile_expression(&stmt.children[0]);
				self.body += &format!("if ({cond}) {{\n");
				self.compile_scope(&*scope);
				self.body += &format!("}}\n");
				if let Some(else_stmt) = else_stmt {
					self.body += &format!("else {{\n");
					self.compile_statement(else_stmt);
					self.body += &format!("}}\n");
				}
			}
			StatementKind::Block(scope) => {
				self.body += &format!("{{\n");
				self.compile_scope(scope);
				self.body += &format!("}}\n");
			}
			k => todo!("{k:?}"),
		}
	}

	fn compile_expression(&mut self, expr: &Expression) -> String {
		match &expr.kind {
			ExpressionKind::NumberLiteral(n) => self.allocate_value_and_set(
				if expr.value_type == ast::BUILTIN_TYPE_INT_LITERAL {
					ast::BUILTIN_TYPE_I32
				} else {
					expr.value_type
				},
				n,
			),
			ExpressionKind::Operator(Operator::Assign) => {
				let left = self.compile_expression(&expr.children[0]);
				let right = self.compile_expression(&expr.children[1]);
				self.body += &format!("(*{left}) = {right};\n");
				// expression results in void, return an empty string
				String::new()
			}
			ExpressionKind::Operator(op) if op.is_binary() => {
				let left = self.compile_expression(&expr.children[0]);
				let right = self.compile_expression(&expr.children[1]);
				let c_op = match op {
					Operator::Add => "+",
					Operator::Equals => "==",
					_ => todo!(),
				};
				self.allocate_value_and_set(expr.value_type, format!("{left} {c_op} {right}"))
			}
			ExpressionKind::Declaration(var) => {
				let value = self.allocate_value(var.ty);
				// TODO: scoped variables
				self.variables.insert(var.name.clone(), value.clone());
				format!("(&{value})")
			}
			ExpressionKind::Identifier(name) => {
				let value = self.variables.get(name).unwrap().clone();
				format!("(&{value})")
			}
			ExpressionKind::Cast => {
				let value = self.compile_expression(&expr.children[0]);
				let from = expr.children[0].value_type;
				let into = expr.value_type;

				if from == into && from.reference && !into.reference {
					// pointer dereference
					format!("(*{value})")
				} else {
					todo!()
				}
			}
			k => todo!("{k:?}"),
		}
	}

	fn allocate_value(&mut self, ty: TypeRef) -> String {
		let name = format!("_{}", self.counter);
		self.counter += 1;
		// TODO: properly convert tack type to c type
		ty.remove_reference();
		self.declarations += &format!("{} {name};\n", ty.formatted(self.ast));
		name
	}

	fn allocate_value_and_set<T: std::fmt::Display>(&mut self, ty: TypeRef, expr: T) -> String {
		let name = self.allocate_value(ty);
		self.body += &format!("{name} = {expr};\n");
		name
	}
}
