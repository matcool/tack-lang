use std::{
	cell::{Cell, RefCell},
	collections::HashMap,
};

use crate::{
	lexer::Operator,
	parser::{Expression, ExpressionKind, Function, Parser, Statement, StatementKind},
};

pub struct Compiler {
	parser: Parser,
	code: RefCell<String>,
	variables: RefCell<HashMap<String, i32>>,
	var_counter: Cell<i32>,
}

impl Compiler {
	pub fn new(parser: Parser) -> Compiler {
		Compiler {
			parser: parser.into(),
			code: String::from("").into(),
			variables: HashMap::new().into(),
			var_counter: 0.into(),
		}
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
		self.variables.borrow_mut().clear();
		self.var_counter.set(0);

		if !function.scope.variables.borrow().is_empty() {
			self.write("push ebp");
			self.write("mov ebp, esp");
			self.write(format!(
				"sub esp, {}",
				function.scope.variables.borrow().len() * 4
			));
		}

		for statement in &function.scope.statements {
			self.compile_statement(&statement.borrow(), function);
		}

		self.generate_return(function);
	}

	fn generate_return(&self, function: &Function) {
		if !function.scope.variables.borrow().is_empty() {
			self.write(format!(
				"add esp, {}",
				function.scope.variables.borrow().len() * 4
			));
			self.write("mov esp, ebp");
			self.write("pop ebp");
		}
		self.write("ret");
	}

	fn compile_statement(&self, statement: &Statement, function: &Function) {
		match statement.kind {
			StatementKind::Return => {
				if let Some(expr) = statement.children.get(0) {
					self.compile_expression(expr, function);
				}
				self.generate_return(function);
			}
			StatementKind::Expression => {
				self.compile_expression(&statement.children[0], function);
			}
		}
	}

	fn compile_expression(&self, exp: &Expression, function: &Function) {
		match exp.kind {
			ExpressionKind::NumberLiteral(number) => {
				self.write(format!("mov eax, {}", number));
			}
			ExpressionKind::BoolLiteral(value) => {
				self.write(format!("mov al, {}", value as u8));
			}
			ExpressionKind::Operator(Operator::Assign) => {
				self.compile_expression(&exp.children[1], function);
				self.write("push eax");
				self.compile_expression(&exp.children[0], function);
				self.write("pop ecx");
				self.write("mov [eax], ecx");
				self.write("mov eax, ecx");
			}
			ExpressionKind::Declaration(ref var) => {
				let val = self.var_counter.get();
				self.variables.borrow_mut().insert(var.name.clone(), val);
				self.write(format!("lea eax, [ebp - {}]", val * 4));
				self.var_counter.set(val + 1)
			}
			ExpressionKind::Variable(ref name) => {
				let val = *self
					.variables
					.borrow()
					.get(name)
					.expect("variable not found in compiler.. how");
				self.write(format!("lea eax, [ebp - {}]", val * 4));
			}
			ExpressionKind::Operator(Operator::Add) => {
				self.compile_expression(&exp.children[0], function);
				self.write("push eax");
				self.compile_expression(&exp.children[1], function);
				self.write("pop ecx");
				self.write("add eax, ecx");
			}
			ExpressionKind::Cast => {
				let child = &exp.children[0];
				self.compile_expression(child, function);
				if !exp.value_type.reference && child.value_type.reference {
					self.write("mov eax, [eax]");
				} else {
					todo!(
						"unhandled cast from {} to {}",
						child.value_type,
						exp.value_type
					);
				}
			}
			ref k => {
				todo!("expression {:?}", k);
			}
		}
	}
}
