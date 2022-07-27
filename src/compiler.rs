use std::{
	cell::{Cell, RefCell},
	collections::HashMap, rc::Rc,
};

use crate::{
	lexer::Operator,
	parser::{Expression, ExpressionKind, Function, Parser, Statement, StatementKind, Scope},
};

pub struct Compiler {
	parser: Parser,
	code: RefCell<String>,
	variables: RefCell<HashMap<String, i32>>,
	var_counter: Cell<i32>,
	label_counter: Cell<i32>,
}

impl Compiler {
	pub fn new(parser: Parser) -> Compiler {
		Compiler {
			parser: parser.into(),
			code: String::from("").into(),
			variables: HashMap::new().into(),
			var_counter: 0.into(),
			label_counter: 0.into(),
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

		if !function.scope.variables.borrow().is_empty() || !function.arguments.is_empty() {
			self.write("push ebp");
			self.write("mov ebp, esp");
			if !function.scope.variables.borrow().is_empty() {
				self.write(format!(
					"sub esp, {}",
					function.scope.variables.borrow().len() * 4
				));
			}
		}

		self.compile_scope(Rc::clone(&function.scope), function);

		self.generate_return(function);
	}

	fn generate_return(&self, function: &Function) {
		let vars = function.scope.variables.borrow();
		if !vars.is_empty() || !function.arguments.is_empty() {
			if !vars.is_empty() {
				self.write(format!(
					"add esp, {}",
					function.scope.variables.borrow().len() * 4
				));
			}
			self.write("mov esp, ebp");
			self.write("pop ebp");
		}
		self.write("ret");
	}

	fn get_label_id(&self) -> i32 {
		let id = self.label_counter.get();
		self.label_counter.set(id + 1);
		id
	}

	fn compile_scope(&self, scope: Rc<Scope>, function: &Function) {
		for statement in &scope.statements {
			self.compile_statement(&statement.borrow(), function);
		}
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
			StatementKind::While(ref scope) => {
				let while_start = format!("_{}", self.get_label_id());
				let while_end = format!("_{}", self.get_label_id());
				self.write(format!("{while_start}:"));
				// condition
				self.compile_expression(&statement.children[0], function);
				self.write("cmp al, 1");
				self.write(format!("jne {while_end}"));
				self.compile_scope(Rc::clone(scope), function);
				self.write(format!("jmp {while_start}"));
				self.write(format!("{while_end}:"));
			}
			StatementKind::If(ref scope) => {
				let if_else = format!("_{}", self.get_label_id());
				let if_end = format!("_{}", self.get_label_id());
				// condition
				self.compile_expression(&statement.children[0], function);
				self.write("cmp al, 1");
				self.write(format!("jne {if_else}"));
				self.compile_scope(Rc::clone(scope), function);
				self.write(format!("jmp {if_end}"));
				self.write(format!("{if_else}:"));
				if let Some(else_branch) = &statement.else_branch {
					self.compile_statement(else_branch, function);
				}
				self.write(format!("{if_end}:"));
			}
			StatementKind::Block(ref scope) => {
				self.compile_scope(Rc::clone(scope), function);
			}
			ref k => {
				todo!("{:?}", k);
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
				if let Some(val) = self.variables.borrow().get(name) {
					self.write(format!("lea eax, [ebp - {}]", val * 4));
				} else {
					// hopefully its a function argument, otherwise type checker haveth failed us
					let index = function
						.arguments
						.iter()
						.enumerate()
						.find(|(_, x)| &x.name == name)
						.expect("expected variable.. type checker went wrong somewhere").0;
					let stack_index = function.arguments.len() - index - 1 + 2;
					self.write(format!("lea eax, [ebp + {}]", stack_index * 4));
				}
			}
			ExpressionKind::Operator(op) if op.is_binary() => {
				self.compile_expression(&exp.children[0], function);
				self.write("push eax");
				self.compile_expression(&exp.children[1], function);
				self.write("pop ecx");
				match op {
					Operator::Add => self.write("add eax, ecx"),
					Operator::Equals | Operator::NotEquals => {
						self.write("cmp eax, ecx");
						if op == Operator::Equals {
							self.write("sete al");
						} else {
							self.write("setne al");
						}
					}
					_ => todo!("unhandled {:?}", op)
				}
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
			ExpressionKind::Call(ref name) => {
				for child in &exp.children {
					self.compile_expression(child, function);
					self.write("push eax");
				}
				self.write(format!("call {name}"));
				let func = self.parser.functions.iter().find(|f| &f.name == name).expect("where the function");
				if !func.arguments.is_empty() {
					self.write(format!("add esp, {}", func.arguments.len() * 4));
				}
			}
			ExpressionKind::Operator(Operator::Reference) => {
				self.compile_expression(&exp.children[0], function);
				// eax should already be a pointer
			}
			ExpressionKind::Operator(Operator::Dereference) => {
				self.compile_expression(&exp.children[0], function);
				self.write("mov eax, [eax]");
			}
			ref k => {
				todo!("expression {:?}", k);
			}
		}
	}
}
