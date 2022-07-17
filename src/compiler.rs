use crate::parser::{Parser, Function, Statement, StatementKind, Expression, ExpressionKind};

pub struct Compiler {
	parser: Parser,
	code: std::cell::RefCell<String>,
}

impl Compiler {
	pub fn new(parser: Parser) -> Compiler {
		Compiler { parser: parser.into(), code: String::from("").into() }
	}

	fn write<T: ToString>(&self, value: T) {
		self.code.borrow_mut().push_str(&value.to_string());
		self.code.borrow_mut().push('\n');
	}

	pub fn compile(&self) -> String {
		for function in &self.parser.functions {
			self.compile_function(function);
		}
		self.code.borrow().clone()
	}

	fn compile_function(&self, function: &Function) {
		self.write(format!("{}:", function.name.clone()));
		
		for statement in &function.statements {
			self.compile_statement(statement, function);
		}
	}

	fn compile_statement(&self, statement: &Statement, function: &Function) {
		match statement.kind {
			StatementKind::Return => {
				if let Some(expr) = statement.children.get(0) {
					self.compile_expression(expr, function);
				}
				self.write("ret");
			},
			StatementKind::Expression => {
				self.compile_expression(&statement.children[0], function);
			}
		}
	}

	fn compile_expression(&self, expression: &Expression, function: &Function) {
		match &expression.kind {
			ExpressionKind::NumberLiteral(number) => {
				self.write(format!("mov eax, {}", number));
			},
			ExpressionKind::BoolLiteral(value) => {
				self.write(format!("mov al, {}", *value as u8));
			},
			ExpressionKind::Variable(_name) => {
				todo!("variables");
			}
		}
	}
}