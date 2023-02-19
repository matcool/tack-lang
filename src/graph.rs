use std::fmt::Write;

use crate::checker::AST;
use crate::parser::{Expression, ExpressionKind, Scope, Statement, StatementKind, TypeRef};

pub struct GraphGen {
	out: String,
	counter: i32,
}

fn escape_rust_for_graphviz(str: &str) -> String {
	str.replace('"', "\\\"").replace('{', "[").replace('}', "]")
}

fn format_type(ast: &AST, type_ref: &TypeRef) -> String {
	if type_ref.is_unknown() {
		return format!("?? {}", if type_ref.reference { "&" } else { "" });
	}
	let ty = ast.get_type(*type_ref);
	format!(
		"{}{}",
		ty.name(ast),
		if type_ref.reference { "&" } else { "" }
	)
}

impl GraphGen {
	fn next_id(&mut self) -> i32 {
		let b = self.counter;
		self.counter += 1;
		b
	}

	pub fn generate_graph(ast: &AST) -> Result<String, std::fmt::Error> {
		let mut obj = GraphGen {
			out: String::new(),
			counter: 0,
		};

		writeln!(obj.out, "digraph G {{")?;
		writeln!(obj.out, "node [shape=record]")?;
		writeln!(obj.out, "rankdir=LR")?;
		for function in &ast.functions {
			let function = &function.borrow();
			let id = obj.next_id();
			writeln!(
				obj.out,
				"node{} [label=\"{}: {}\"]",
				id,
				function.name,
				format_type(ast, &function.return_type)
			)?;
			obj.generate_scope(ast, &function.scope, id)?;
		}
		writeln!(obj.out, "}}")?;

		Ok(obj.out)
	}

	fn generate_scope(
		&mut self,
		ast: &AST,
		scope: &Scope,
		parent_id: i32,
	) -> Result<(), std::fmt::Error> {
		for stmt in &scope.statements {
			let id = self.generate_statement(ast, &stmt.borrow())?;
			// TODO: change arrow shape for parent -> statement connections
			writeln!(self.out, "node{} -> node{} [color=red]", parent_id, id)?;
		}
		Ok(())
	}

	fn generate_statement(&mut self, ast: &AST, stmt: &Statement) -> Result<i32, std::fmt::Error> {
		let id = self.next_id();
		let name = {
			let x = format!("{:?}", stmt.kind);
			// incredible
			x.split_once('(').map_or(x.clone(), |(x, _)| x.to_string())
		};

		writeln!(self.out, "node{} [color=coral3 label=\"{}\"]", id, name)?;
		for exp in &stmt.children {
			let child_id = self.generate_expression(ast, exp)?;
			writeln!(self.out, "node{} -> node{}", id, child_id)?;
		}
		match &stmt.kind {
			StatementKind::While(ref scope)
			| StatementKind::If(ref scope)
			| StatementKind::Block(ref scope) => {
				self.generate_scope(ast, scope, id)?;
			}
			_ => {}
		}
		if let Some(else_branch) = &stmt.else_branch {
			let child_id = self.generate_statement(ast, else_branch)?;
			writeln!(
				self.out,
				"node{} -> node{} [label=\"else\" color=red]",
				id, child_id
			)?;
		}
		Ok(id)
	}

	fn generate_expression(&mut self, ast: &AST, exp: &Expression) -> Result<i32, std::fmt::Error> {
		let id = self.next_id();
		write!(self.out, "node{} [color=darkgreen label=\"", id)?;
		match &exp.kind {
			ExpressionKind::Declaration(var) => {
				write!(
					self.out,
					"Declaration({}, {})",
					var.name,
					format_type(ast, &var.ty)
				)?;
			}
			ExpressionKind::Variable(name) => {
				write!(self.out, "Variable({name})")?;
			}
			ExpressionKind::Call(name) => {
				write!(self.out, "Call({name})")?;
			}
			ExpressionKind::StructAccess(struct_ref, field) => {
				write!(
					self.out,
					"StructAccess({}, {})",
					format_type(ast, struct_ref),
					field
				)?;
			}
			kind => {
				write!(
					self.out,
					"{}",
					escape_rust_for_graphviz(&format!("{:?}", kind))
				)?;
			}
		}
		writeln!(self.out, " | {}\"]", format_type(ast, &exp.value_type))?;
		for child in &exp.children {
			let child_id = self.generate_expression(ast, child)?;
			writeln!(self.out, "node{} -> node{}", id, child_id)?;
		}
		Ok(id)
	}
}
