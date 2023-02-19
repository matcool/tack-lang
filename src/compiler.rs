use std::{
	cell::{Cell, RefCell},
	collections::HashMap,
	rc::Rc,
};

use itertools::Itertools;

use crate::{
	checker::AST,
	lexer::Operator,
	parser::{
		BuiltInType, Expression, ExpressionKind, Function, Scope, Statement, StatementKind, Type,
		TypeRef,
	},
};

pub struct Compiler {
	ast: AST,
	code: RefCell<String>,
	variables: RefCell<HashMap<String, i32>>,
	var_counter: Cell<i32>,
	label_counter: Cell<i32>,
	string_literals: RefCell<Vec<String>>,
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
				BuiltInType::IntLiteral => unreachable!("this shouldnt be called"),
			},
			Type::Pointer(_) => POINTER_SIZE,
			Type::Struct(stru) => stru
				.fields
				.iter()
				.map(|field| ast.get_type_size(field.ty))
				.sum(),
			Type::Array(inner, size) => ast.get_type_size(*inner) * size,
		}
	}
	pub fn aligned_size(&self, ast: &AST) -> usize {
		let size = self.size(ast);
		if size % POINTER_SIZE == 0 {
			size
		} else {
			size + (POINTER_SIZE - (size % POINTER_SIZE))
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

impl Function {
	fn needs_cleanup(&self) -> bool {
		*self.scope_size.borrow() != 0 || !self.arguments.is_empty() || self.is_struct_return
	}
	fn arguments_size(&self, ast: &AST) -> usize {
		self.arguments
			.iter()
			.map(|arg| ast.get_type(arg.ty).aligned_size(ast))
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
			string_literals: vec![].into(),
		}
	}

	fn write<T: ToString>(&self, value: T) {
		self.code.borrow_mut().push_str(&value.to_string());
		self.code.borrow_mut().push('\n');
	}

	pub fn compile(&self) -> String {
		for function in &self.ast.functions {
			let function = &function.borrow();
			if function.is_extern {
				self.write(format!("extern {}", function.name));
			} else {
				self.compile_function(function);
			}
		}
		for (i, str) in self.string_literals.borrow().iter().enumerate() {
			self.write(format!(
				"_str{}: db {}",
				i,
				str.chars().map(|x| x as u8).join(",")
			));
		}

		self.code.borrow().clone()
	}

	fn compile_function(&self, function: &Function) {
		self.write(format!("{}:", function.name.clone()));
		self.variables.borrow_mut().clear();
		self.var_counter.set(0);

		let scope_size = function.scope.size(&self.ast)
			+ function
				.scope
				.children
				.borrow()
				.iter()
				.map(|s| s.size(&self.ast))
				.sum::<usize>();
		*function.scope_size.borrow_mut() = scope_size;

		if function.needs_cleanup() {
			self.write("push ebp");
			self.write("mov ebp, esp");
			if scope_size != 0 {
				self.write(format!("sub esp, {}", scope_size));
			}
		}

		self.compile_scope(Rc::clone(&function.scope), function);

		self.generate_return(function);
	}

	fn generate_return(&self, function: &Function) {
		let scope_size = *function.scope_size.borrow();
		if function.needs_cleanup() {
			if scope_size != 0 {
				self.write(format!("add esp, {}", scope_size));
			}
			if function.is_struct_return {
				self.write("mov ecx, [ebp + 8]");
				self.copy_memory(
					"ecx",
					"eax",
					"edx",
					self.ast.get_type_size(function.return_type),
				);
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

	fn allocate_variable(&self, ty: TypeRef) -> i32 {
		let size = self.ast.get_type_size(ty);
		let offset = self.var_counter.get() + size as i32;
		self.var_counter.set(offset);
		offset
	}

	fn compile_scope(&self, scope: Rc<Scope>, function: &Function) {
		for statement in &scope.statements {
			self.compile_statement(&statement.borrow(), function);
		}
	}

	fn copy_memory(&self, dst_reg: &str, src_reg: &str, scratch_reg: &str, size: usize) {
		self.write(format!(
			"; copying {size} bytes from {src_reg} to {dst_reg}, using {scratch_reg} as scratch"
		));
		let times = size / POINTER_SIZE;
		for i in 0..times {
			let i = i * POINTER_SIZE;
			self.write(format!("mov {scratch_reg}, [{src_reg} + {i}]"));
			self.write(format!("mov [{dst_reg} + {i}], {scratch_reg}"));
		}
		let scratch_reg = match scratch_reg {
			"eax" => "al",
			"ebx" => "bl",
			"ecx" => "cl",
			"edx" => "dl",
			_ => unreachable!(),
		};
		for i in (times * POINTER_SIZE)..size {
			self.write(format!("mov {scratch_reg}, BYTE [{src_reg} + {i}]"));
			self.write(format!("mov BYTE [{dst_reg} + {i}], {scratch_reg}"));
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
			#[allow(unreachable_patterns)]
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

				if self.ast.is_struct_or_array(exp.children[0].value_type) {
					self.copy_memory("eax", "ecx", "edx", lhs_size);
				} else {
					match lhs_size {
						1 => self.write("mov BYTE [eax], cl"),
						2 => self.write("mov WORD [eax], cx"),
						4 => self.write("mov [eax], ecx"),
						_ => unimplemented!("assignment for type with size {}", lhs_size),
					};
				}
			}
			ExpressionKind::Declaration(ref var) => {
				let val = self.allocate_variable(var.ty);
				self.variables.borrow_mut().insert(var.name.clone(), val);
				self.write(format!("lea eax, [ebp - {}]", val));
			}
			ExpressionKind::Variable(ref name) => {
				if let Some(val) = self.variables.borrow().get(name) {
					self.write(format!("lea eax, [ebp - {}]", val));
				} else {
					// hopefully its a function argument, otherwise type checker haveth failed us
					let arg_index = function
						.arguments
						.iter()
						.enumerate()
						.find(|(_, x)| &x.name == name)
						.expect("expected variable.. type checker went wrong somewhere")
						.0;
					// stack looks like this
					// ↓ ebp
					// old_ebp ret_ptr struct_ret_ptr? arg_2 arg_1 arg_0
					let mut stack_index = 2 * POINTER_SIZE;
					if function.is_struct_return {
						stack_index += POINTER_SIZE;
					}
					for arg in function.arguments.iter().skip(arg_index + 1) {
						stack_index += self.ast.get_type(arg.ty).aligned_size(&self.ast);
					}
					self.write(format!("lea eax, [ebp + {}]", stack_index));
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
			ExpressionKind::Operator(op @ (Operator::And | Operator::Or)) => {
				let skip_label = format!("_{}", self.get_label_id());

				self.compile_expression(&exp.children[0], function);
				if op == Operator::And {
					self.write("cmp al, 0");
				} else {
					self.write("cmp al, 1");
				}
				self.write(format!("je {skip_label}"));
				self.compile_expression(&exp.children[1], function);
				self.write(format!("{skip_label}:"));
			}
			ExpressionKind::Operator(op) if op.is_binary() => {
				self.compile_expression(&exp.children[0], function);
				self.write("push eax");
				self.compile_expression(&exp.children[1], function);
				self.write("pop ecx");
				// ecx is lhs
				// eax is rhs
				if let Type::Pointer(inner) = self.ast.get_type(exp.children[0].value_type).as_ref()
				{
					self.write(format!("imul eax, {}", self.ast.get_type_size(*inner)));
				}
				match op {
					Operator::Add => self.write("add eax, ecx"),
					Operator::Sub => {
						self.write("sub ecx, eax");
						self.write("mov eax, ecx");
					}
					Operator::Equals
					| Operator::NotEquals
					| Operator::LessThan
					| Operator::LessThanEq
					| Operator::GreaterThan
					| Operator::GreaterThanEq => {
						self.write("cmp ecx, eax");
						match op {
							Operator::Equals => self.write("sete al"),
							Operator::NotEquals => self.write("setne al"),
							Operator::LessThan => self.write("setl al"),
							Operator::LessThanEq => self.write("setle al"),
							Operator::GreaterThan => self.write("setg al"),
							Operator::GreaterThanEq => self.write("setge al"),
							_ => unreachable!(),
						}
					}
					Operator::Multiply => {
						// edx:eax = eax * ecx
						self.write("imul ecx");
					}
					Operator::Divide | Operator::Mod => {
						self.write("xchg ecx, eax");
						self.write("mov edx, 0");
						self.write("idiv ecx");
						if op == Operator::Mod {
							self.write("mov eax, edx");
						}
					}
					Operator::BitAnd => {
						self.write("and eax, ecx");
					}
					Operator::BitOr => {
						self.write("or eax, ecx");
					}
					Operator::BitShiftLeft | Operator::BitShiftRight => {
						self.write("xchg ecx, eax");
						if op == Operator::BitShiftLeft {
							self.write("shl eax, cl");
						} else {
							self.write("shr eax, cl");
						}
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
					match self.ast.get_type_size(to) {
						4 => self.write("mov eax, [eax]"),
						2 => self.write("mov ax, WORD [eax]\nand eax, 0xffff"),
						1 => self.write("mov al, BYTE [eax]\nand eax, 0xff"),
						_ => todo!(),
					}
				} else {
					let from = self.ast.get_type(from);
					let to = self.ast.get_type(to);

					match (from.as_ref(), to.as_ref()) {
						(
							Type::BuiltIn(_),
							Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8 | BuiltInType::UPtr),
						) => {
							let from_size = from.size(&self.ast);
							let to_size = to.size(&self.ast);
							if from_size == 1 && to_size > from_size {
								self.write("and eax, 0xff");
							}
						}
						(Type::Pointer(_), Type::BuiltIn(BuiltInType::UPtr)) => {}
						(Type::Pointer(_), Type::Pointer(_)) => {}
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
			ExpressionKind::Call(ref name) if name == "syscall" => {
				if exp.children.len() == 7 {
					self.write("push ebp");
				}
				for child in &exp.children {
					self.compile_expression(child, function);
					self.write("push eax");
				}

				let regs = ["eax", "ebx", "ecx", "edx", "esi", "edi", "ebp"];

				for i in 0..exp.children.len() {
					let reg = regs[exp.children.len() - i - 1];
					self.write(format!("mov {reg}, [esp + {}]", i * 4));
				}
				self.write("int 0x80");
				// FIXME: syscalls return in eax and edx
				// but this only cares about the value in eax, for now

				self.write(format!("add esp, {}", exp.children.len() * 4));
				if exp.children.len() == 7 {
					self.write("pop ebp");
				}
			}
			ExpressionKind::Call(ref name) => {
				for child in &exp.children {
					self.compile_expression(child, function);
					if self.ast.is_struct_or_array(child.value_type) {
						let size = self.ast.get_type_size(child.value_type);
						self.write(format!(
							"sub esp, {}",
							self.ast.get_type(child.value_type).aligned_size(&self.ast)
						));
						self.copy_memory("esp", "eax", "ecx", size);
					} else {
						self.write("push eax");
					}
				}
				let func = self
					.ast
					.functions
					.iter()
					.find(|f| &f.borrow().name == name)
					.expect("where the function")
					.borrow();

				let mut struct_offset = 0;
				if func.is_struct_return {
					struct_offset = self.allocate_variable(func.return_type);

					// cheat and just put a pointer to this on eax
					self.write(format!("lea eax, [ebp - {}]", struct_offset));
					self.write("push eax");
				}

				self.write(format!("call {name}"));

				if func.is_struct_return {
					self.write("add esp, 4");
					self.write(format!("lea eax, [ebp - {}]", struct_offset));
				}
				if !func.arguments.is_empty() {
					self.write(format!("add esp, {}", func.arguments_size(&self.ast)));
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
			ExpressionKind::Operator(Operator::Negate) => {
				self.compile_expression(&exp.children[0], function);
				self.write("neg eax");
			}
			ExpressionKind::AsmLiteral(ref str) => {
				self.write(str);
			}
			ExpressionKind::StringLiteral(ref str) => {
				let lits = &mut self.string_literals.borrow_mut();
				let index = lits.len();
				lits.push(str.clone());

				let offset = self.allocate_variable(exp.value_type);

				self.write(format!("lea eax, [_str{}]", index));
				self.write(format!("mov [ebp - {}], eax", offset));
				self.write(format!(
					"mov DWORD [ebp - {}], {}",
					offset - POINTER_SIZE as i32,
					str.len()
				));
				// cheat and just put a pointer to this on eax
				self.write(format!("lea eax, [ebp - {}]", offset));
			}
			ExpressionKind::ArrayLiteral => {
				let stack_offset = self.allocate_variable(exp.value_type);

				let size = self.ast.get_type_size(exp.children[0].value_type);
				for (i, child) in exp.children.iter().enumerate() {
					self.compile_expression(child, function);
					self.write(format!(
						"mov [ebp - {}], eax",
						stack_offset - (i * size) as i32
					));
				}

				self.write(format!("lea eax, [ebp - {}]", stack_offset));
			}
			ExpressionKind::ArrayIndex => {
				self.compile_expression(&exp.children[0], function);
				self.write("push eax");
				self.compile_expression(&exp.children[1], function);
				self.write(format!(
					"imul eax, {}",
					self.ast.get_type_size(exp.value_type)
				));
				self.write("pop ecx");
				self.write("add eax, ecx");
			}
			ref k => {
				todo!("expression {:?}", k);
			}
		}
	}
}
