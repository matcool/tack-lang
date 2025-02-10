use std::collections::HashMap;

use itertools::Itertools;

use crate::{
	ast::{
		BuiltInType, Expression, ExpressionKind, Function, Scope, Statement, StatementKind,
		StructType, Type, TypeRef, AST, BUILTIN_TYPE_BOOL, BUILTIN_TYPE_STR,
	},
	lexer::Operator,
};

pub struct Compiler<'a> {
	ast: &'a AST,
	declarations: String,
	body: String,
	counter: i32,
	variables: HashMap<usize, String>,
	struct_defitions: String,
	struct_counter: i32,
	generated_arrays: HashMap<(TypeRef, usize), String>,
}

impl Compiler<'_> {
	pub fn new(ast: &AST) -> Compiler {
		Compiler {
			ast,
			declarations: Default::default(),
			body: Default::default(),
			struct_defitions: Default::default(),
			counter: 0,
			variables: Default::default(),
			struct_counter: 0,
			generated_arrays: Default::default(),
		}
	}

	pub fn compile(mut self) -> String {
		let header = "\
#include <stdbool.h>
#include <stdint.h>
typedef int32_t i32;
typedef uint8_t u8;
typedef uintptr_t uptr;\n";

		for ty in &self.ast.types {
			if let Type::Struct(struct_type) = ty {
				self.add_struct(struct_type);
			}
		}

		let mut functions = String::new();
		for function in &self.ast.functions {
			functions += &self.compile_function(function);
		}
		[header, &self.struct_defitions, &functions].join("\n")
	}

	fn add_struct(&mut self, struct_type: &StructType) -> String {
		self.struct_defitions += &format!("struct {} {{\n", struct_type.name);
		for field in &struct_type.fields {
			// TODO: array fields
			let fmt = self.format_type(field.ty);
			self.struct_defitions += &format!("{} {};\n", fmt, field.name);
		}
		self.struct_defitions += "};\n";

		struct_type.name.clone()
	}

	fn add_array(&mut self, ty: TypeRef) -> String {
		let Type::Array(inner, size) = *self.ast.get_type(ty) else {
			panic!("ty is not an array")
		};
		if let Some(existing) = self.generated_arrays.get(&(inner, size)) {
			return "struct ".to_string() + existing;
		}

		let name = format!("Arr{}", self.struct_counter);
		let inner_fmt = self.format_type(inner);
		self.struct_defitions += &format!("struct {name} {{ {inner_fmt} data[{size}]; }};\n");
		self.struct_counter += 1;
		self.generated_arrays.insert((inner, size), name.clone());
		return "struct ".to_string() + &name;
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

		let args = function
			.arguments
			.iter()
			.map(|arg| {
				let (ty, name) = self.allocate_var_raw(arg.ty);
				self.variables.insert(arg.unique_id, name.clone());
				format!("{ty} {name}")
			})
			.join(", ");

		output += &format!(
			"{} {}({args})",
			self.format_type(function.return_type),
			function.name
		);

		if function.attributes.is_c_extern {
			output += ";\n";
		} else {
			output += " {\n";
			self.compile_scope(&function.scope);
			output += &self.declarations;
			output += &self.body;
			output += "}\n";
		}

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
				let value = self.compile_expression(&stmt.children[0]);
				self.body += &format!("{value};\n");
			}
			StatementKind::If(scope, else_stmt) => {
				let cond = self.compile_expression(&stmt.children[0]);
				self.body += &format!("if ({cond}) {{\n");
				self.compile_scope(scope);
				self.body += &format!("}}\n");
				if let Some(else_stmt) = else_stmt {
					self.body += &format!("else {{\n");
					self.compile_statement(else_stmt);
					self.body += &format!("}}\n");
				}
			}
			StatementKind::While(scope) => {
				let condition_var = self.allocate_value(BUILTIN_TYPE_BOOL);
				let cond = self.compile_expression(&stmt.children[0]);
				self.body += &format!("{condition_var} = {cond};\n");
				self.body += &format!("while ({condition_var}) {{\n");
				self.compile_scope(scope);
				let cond = self.compile_expression(&stmt.children[0]);
				self.body += &format!("{condition_var} = {cond};\n");
				self.body += &format!("}}\n");
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
			ExpressionKind::NumberLiteral(n) => format!("({n})"),
			ExpressionKind::BoolLiteral(b) => format!("({b})"),
			ExpressionKind::StringLiteral(str) => {
				// TODO: properly escape string literal, or just array it
				format!("((struct str){{{str:?}, {}}})", str.len())
			}
			ExpressionKind::Operator(Operator::Assign) => {
				let left = self.compile_expression(&expr.children[0]);
				let right = self.compile_expression(&expr.children[1]);
				self.body += &format!("(*{left}) = {right}");
				// expression results in void, return an empty string
				String::new()
			}
			ExpressionKind::Operator(op) if op.is_binary() => {
				let left = self.compile_expression(&expr.children[0]);
				let right = self.compile_expression(&expr.children[1]);
				let c_op = match op {
					Operator::Add => "+",
					Operator::Sub => "-",
					Operator::Multiply => "*",
					Operator::Divide => "/",
					Operator::Mod => "%",
					Operator::And => "&&",
					Operator::Or => "||",
					Operator::BitAnd => "&",
					Operator::BitOr => "|",
					Operator::BitShiftLeft => "<<",
					Operator::BitShiftRight => ">>",
					Operator::GreaterThan => ">",
					Operator::GreaterThanEq => ">=",
					Operator::LessThan => "<",
					Operator::LessThanEq => "<=",
					Operator::Equals => "==",
					Operator::NotEquals => "!=",
					_ => unreachable!("{op:?} should not be here"),
				};
				self.allocate_value_and_set(expr.value_type, format!("{left} {c_op} {right}"))
			}
			ExpressionKind::Operator(Operator::Negate) => {
				let value = self.compile_expression(&expr.children[0]);
				format!("(-{value})")
			}
			ExpressionKind::Operator(Operator::Reference | Operator::Dereference) => {
				// since we store ref types as pointers, this does not need to do anything
				self.compile_expression(&expr.children[0])
			}
			ExpressionKind::Declaration(var) => {
				let value = self.allocate_value(var.ty);
				self.variables.insert(var.unique_id, value.clone());
				format!("(&{value})")
			}
			ExpressionKind::Identifier(var) => {
				let value = self.variables.get(&var.unique_id).unwrap().clone();
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
					let from_type = self.ast.get_type(from);
					let into_type = self.ast.get_type(into);
					match (from_type, into_type) {
						(
							Type::BuiltIn(
								BuiltInType::I32
								| BuiltInType::U8
								| BuiltInType::Bool
								| BuiltInType::UPtr,
							),
							Type::BuiltIn(BuiltInType::U8 | BuiltInType::I32 | BuiltInType::UPtr),
						) => value,
						(
							Type::BuiltIn(BuiltInType::I32 | BuiltInType::U8 | BuiltInType::UPtr),
							Type::BuiltIn(BuiltInType::Bool),
						) => value,
						(Type::Pointer(_), Type::Pointer(_))
						| (Type::Pointer(_), Type::BuiltIn(BuiltInType::UPtr))
						| (Type::BuiltIn(BuiltInType::UPtr), Type::Pointer(_)) => {
							format!("(({}){value})", self.format_type(into))
						}
						_ => todo!("{} -> {}", self.format_type(from), self.format_type(into)),
					}
				}
			}
			ExpressionKind::StructAccess(name) => {
				let mut value = self.compile_expression(&expr.children[0]);
				if expr.children[0].value_type.reference {
					value = format!("(*{value})");
				}
				value = format!("({value}.{name})");
				if expr.value_type.reference {
					value = format!("(&{value})");
				}
				value
			}
			ExpressionKind::ArrayLiteral => {
				let elements = expr
					.children
					.iter()
					.map(|e| self.compile_expression(e))
					.join(", ");
				format!("(({}){{{elements}}})", self.add_array(expr.value_type))
			}
			ExpressionKind::ArrayIndex => {
				let mut arr = self.compile_expression(&expr.children[0]);
				if self.ast.is_array(expr.children[0].value_type) {
					arr = format!("{arr}.data");
				}
				let index = self.compile_expression(&expr.children[1]);
				format!("(&({arr})[{index}])")
			}
			ExpressionKind::Call(func_name) => {
				let args = expr
					.children
					.iter()
					.map(|child| self.compile_expression(child))
					.join(", ");

				format!("({func_name}({args}))")
			}
			k => todo!("{k:?}"),
		}
	}

	fn allocate_value(&mut self, ty: TypeRef) -> String {
		let (type_str, name) = self.allocate_var_raw(ty);
		self.declarations += &format!("{type_str} {name};\n");
		name
	}

	fn allocate_value_and_set<T: std::fmt::Display>(&mut self, ty: TypeRef, expr: T) -> String {
		let name = self.allocate_value(ty);
		self.body += &format!("{name} = {expr};\n");
		name
	}

	fn allocate_var_raw(&mut self, ty: TypeRef) -> (String, String) {
		let name = format!("_{}", self.counter);
		self.counter += 1;
		let formatted = self.format_type(ty);
		(formatted, name)
	}

	fn format_type(&mut self, ty: TypeRef) -> String {
		// TODO: properly convert tack type to c type
		if self.ast.is_array(ty) {
			return self.add_array(ty);
		}
		match self.ast.get_type(ty) {
			Type::Pointer(inner) => format!("{}*", self.format_type(*inner)),
			Type::Struct(struct_type) => format!("struct {}", struct_type.name),
			_ => ty.remove_reference().formatted(self.ast),
		}
	}
}
