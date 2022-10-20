use std::{
	cell::{Cell, RefCell},
	collections::HashMap,
	rc::Rc,
};

use crate::{
	checker::AST,
	lexer::Operator,
	parser::{
		BuiltInType, Expression, ExpressionKind, Function, Scope, Statement, StatementKind, Type,
	},
};

pub struct Compiler {
	ast: AST,
	code: RefCell<String>,
	variables: RefCell<HashMap<String, i32>>,
	var_counter: Cell<i32>,
	label_counter: Cell<i32>,
}

const POINTER_SIZE: usize = 4;

impl Type {
	pub fn size(&self, ast: &AST) -> usize {
		match self {
			Type::BuiltIn(built_in) => match built_in {
				BuiltInType::I32 => 4,
				BuiltInType::U8 => 1,
				BuiltInType::UPtr => POINTER_SIZE,
				BuiltInType::Bool => 1,
				BuiltInType::Void => 0,
			},
			Type::Pointer(_) => POINTER_SIZE,
			Type::Struct(stru) => stru
				.fields
				.iter()
				.map(|field| ast.get_type_size(field.ty))
				.sum(),
		}
	}
}

impl Scope {
	fn size(&self, ast: &AST) -> usize {
		self.variables
			.borrow()
			.iter()
			.map(|var| ast.get_type_size(var.ty))
			.sum()
	}
}

impl Compiler {
	pub fn new(ast: AST) -> Compiler {
		Compiler {
			ast,
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
		for function in &self.ast.functions {
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
				// TODO: save this in the function as to not calculate it every time
				let scope_size = function.scope.size(&self.ast);
				self.write(format!("sub esp, {}", scope_size));
			}
		}

		self.compile_scope(Rc::clone(&function.scope), function);

		self.generate_return(function);
	}

	fn generate_return(&self, function: &Function) {
		let vars = function.scope.variables.borrow();
		if !vars.is_empty() || !function.arguments.is_empty() {
			if !vars.is_empty() {
				let scope_size = function.scope.size(&self.ast);
				self.write(format!("add esp, {}", scope_size));
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
		self.write("\n; Statement:");
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
		self.write(format!("; {:?}", exp.kind));
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

				let lhs_size = self.ast.get_type_size(exp.children[0].value_type);
				match lhs_size {
					1 => self.write("mov BYTE [eax], cl"),
					2 => self.write("mov WORD [eax], cx"),
					4 => self.write("mov [eax], ecx"),
					_ => unimplemented!("assignment for type with size {}", lhs_size),
				};

				self.write("mov eax, ecx");
			}
			ExpressionKind::Declaration(ref var) => {
				let size = self.ast.get_type_size(var.ty);
				let val = self.var_counter.get() + size as i32;
				self.var_counter.set(val);
				self.variables.borrow_mut().insert(var.name.clone(), val);
				self.write(format!("lea eax, [ebp - {}]", val));
			}
			ExpressionKind::Variable(ref name) => {
				if let Some(val) = self.variables.borrow().get(name) {
					self.write(format!("lea eax, [ebp - {}]", val));
				} else {
					// hopefully its a function argument, otherwise type checker haveth failed us
					let index = function
						.arguments
						.iter()
						.enumerate()
						.find(|(_, x)| &x.name == name)
						.expect("expected variable.. type checker went wrong somewhere")
						.0;
					let stack_index = function.arguments.len() - index - 1 + 2;
					self.write(format!("lea eax, [ebp + {}]", stack_index * 4));
				}
			}
			ExpressionKind::Operator(Operator::Dot) => {
				unreachable!("shouldnt be getting operator dot in compiler!");
			}
			ExpressionKind::StructAccess(struct_type_ref, ref field_name) => {
				self.compile_expression(&exp.children[0], function);
				let ty = self.ast.get_type(struct_type_ref);
				if let Type::Struct(stru) = ty.as_ref() {
					let mut offset = 0;
					for field in &stru.fields {
						if &field.name == field_name {
							self.write(format!("lea eax, [eax + {}]", offset));
							break;
						} else {
							offset += self.ast.get_type_size(field.ty);
						}
					}
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
					},
					Operator::Divide | Operator::Mod => {
						self.write("xchg ecx, eax");
						self.write("mov edx, 0");
						self.write("idiv ecx");
						if op == Operator::Mod {
							self.write("mov eax, edx");
						}
					},
					Operator::Sub => {
						self.write("sub ecx, eax");
						self.write("mov eax, ecx");
					}
					_ => todo!("unhandled {:?}", op),
				}
			}
			ExpressionKind::Cast => {
				let child = &exp.children[0];
				self.compile_expression(child, function);
				let from = child.value_type;
				let to = exp.value_type;
				if from == to && !to.reference && from.reference {
					self.write("mov eax, [eax]");
				} else {
					let from = self.ast.get_type(from);
					let to = self.ast.get_type(to);

					match (from.as_ref(), to.as_ref()) {
						(
							Type::BuiltIn(_),
							Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8 | BuiltInType::UPtr),
						) => {}
						(Type::Pointer(_), Type::BuiltIn(BuiltInType::UPtr)) => {}
						(Type::BuiltIn(BuiltInType::UPtr), Type::Pointer(_)) => {}
						(Type::BuiltIn(_), Type::BuiltIn(BuiltInType::Bool)) => {
							self.write("cmp eax, 0");
							self.write("setne al");
						}
						_ => {
							todo!("unhandled cast from {:?} to {:?}", from, to);
						}
					}
				}
			}
			ExpressionKind::Call(ref name) => {
				for child in &exp.children {
					self.compile_expression(child, function);
					self.write("push eax");
				}
				self.write(format!("call {name}"));
				let func = self
					.ast
					.functions
					.iter()
					.find(|f| &f.name == name)
					.expect("where the function");
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
				// since this evaluates to a reference,
				// just keep it as a pointer
			}
			ExpressionKind::AsmLiteral(ref str) => {
				self.write(str);
			}
			ref k => {
				todo!("expression {:?}", k);
			}
		}
	}
}
