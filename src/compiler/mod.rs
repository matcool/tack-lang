use std::collections::HashMap;

use itertools::Itertools;

use crate::{
	ast::{
		BuiltInType, Expression, ExpressionKind, Function, FunctionKey, Scope, Statement,
		StatementKind, StructType, Type, TypeRef, AST, BUILTIN_TYPE_BOOL,
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
		let header = "#include <tack_runtime.h>\n";

		for ty in &self.ast.types {
			if let Type::Struct(struct_type) = ty {
				self.add_struct(struct_type);
			}
		}

		let mut function_declarations = String::new();
		for function in self.ast.functions.values() {
			function_declarations += &self.compile_function_decl(function);
			function_declarations += ";\n";
		}

		let mut functions = String::new();
		for function in self.ast.functions.values() {
			if !function.is_external() {
				functions += &self.compile_function(function);
			}
		}
		[
			header,
			&self.struct_defitions,
			&function_declarations,
			&functions,
		]
		.join("\n")
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
		"struct ".to_string() + &name
	}

	fn reset_values(&mut self) {
		self.declarations = String::new();
		self.body = String::new();
		self.counter = 0;
		self.variables = HashMap::new();
	}

	fn compile_function_decl(&mut self, function: &Function) -> String {
		let mut output = String::new();

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
			self.mangle_function(function.key)
		);

		output
	}

	fn compile_function(&mut self, function: &Function) -> String {
		self.reset_values();

		let mut output = self.compile_function_decl(function);

		if function.is_external() {
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
			StatementKind::Return(value) => {
				if let Some(value) = value {
					let value = self.compile_expression(value);
					self.body += &format!("return {value};\n");
				} else {
					self.body += "return;\n";
				}
			}
			StatementKind::Expression(expr) => {
				let value = self.compile_expression(expr);
				self.body += &format!("{value};\n");
			}
			StatementKind::If(scope, cond, else_stmt) => {
				let cond = self.compile_expression(cond);
				self.body += &format!("if ({cond}) {{\n");
				self.compile_scope(scope);
				self.body += "}\n";
				if let Some(else_stmt) = else_stmt {
					self.body += "else {\n";
					self.compile_statement(else_stmt);
					self.body += "}\n";
				}
			}
			StatementKind::While(scope, cond_expr) => {
				let condition_var = self.allocate_value(BUILTIN_TYPE_BOOL);
				let cond = self.compile_expression(cond_expr);
				self.body += &format!("{condition_var} = {cond};\n");
				self.body += &format!("while ({condition_var}) {{\n");
				self.compile_scope(scope);
				let cond = self.compile_expression(cond_expr);
				self.body += &format!("{condition_var} = {cond};\n");
				self.body += "}\n";
			}
			StatementKind::Block(scope) => {
				self.body += "{\n";
				self.compile_scope(scope);
				self.body += "}\n";
			}
		}
	}

	fn compile_expression(&mut self, expr: &Expression) -> String {
		match &expr.kind {
			ExpressionKind::NumberLiteral(n) => format!("({n})"),
			ExpressionKind::BoolLiteral(b) => format!("({b})"),
			ExpressionKind::StringLiteral(str) => {
				format!(
					"((struct str){{(u8[{}]){{{}}}, {}}})",
					str.len() + 1, // just hack in the null terminator for now
					str.bytes().chain([0].into_iter()).join(", "),
					str.len()
				)
			}
			ExpressionKind::BinaryOperator(Operator::Assign, left, right) => {
				let left = self.compile_expression(left);
				let right = self.compile_expression(right);
				self.body += &format!("(*{left}) = {right}");
				// expression results in void, return an empty string
				String::new()
			}
			ExpressionKind::BinaryOperator(op, left, right) if op.is_binary() => {
				let left = self.compile_expression(left);
				let right = self.compile_expression(right);
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
				self.allocate_value_and_set(expr.ty, format!("{left} {c_op} {right}"))
			}
			ExpressionKind::UnaryOperator(Operator::Negate, value) => {
				let value = self.compile_expression(value);
				format!("(-{value})")
			}
			ExpressionKind::UnaryOperator(Operator::Reference | Operator::Dereference, value) => {
				// since we store ref types as pointers, this does not need to do anything
				self.compile_expression(value)
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
			ExpressionKind::Cast(child) => {
				let value = self.compile_expression(child);
				let from = child.ty;
				let into = expr.ty;

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
			ExpressionKind::StructAccess(child, name) => {
				let mut value = self.compile_expression(child);
				if child.ty.reference {
					value = format!("(*{value})");
				}
				value = format!("({value}.{name})");
				if expr.ty.reference {
					value = format!("(&{value})");
				}
				value
			}
			ExpressionKind::ArrayLiteral(values) => {
				let elements = values.iter().map(|e| self.compile_expression(e)).join(", ");
				format!("(({}){{{elements}}})", self.add_array(expr.ty))
			}
			ExpressionKind::ArrayIndex(arr_expr, index) => {
				let mut arr = self.compile_expression(arr_expr);
				if self.ast.is_array(arr_expr.ty) {
					arr = format!("{arr}.data");
				}
				let index = self.compile_expression(index);
				format!("(&({arr})[{index}])")
			}
			ExpressionKind::Call(func_key, args) => {
				let func_name = self.mangle_function(*func_key);
				let args = args
					.iter()
					.map(|child| self.compile_expression(child))
					.join(", ");

				format!("({func_name}({args}))")
			}
			ExpressionKind::StructLiteral(values) => {
				let value = self.allocate_value(expr.ty);
				for (name, expr) in values {
					let child = self.compile_expression(expr);
					self.body += &format!("{value}.{name} = {child};\n");
				}
				value
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

	fn mangle_function(&mut self, key: FunctionKey) -> String {
		let function = &self.ast.functions[key];
		let mut res = String::new();
		let mut ns = function.parent;
		while ns != self.ast.global {
			res = format!("{}${res}", self.ast.namespaces[ns].name);
			ns = self.ast.namespaces[ns].parent;
		}
		res + &function.name
	}
}
