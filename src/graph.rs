use std::fmt::Write;

use crate::parser::{Expression, ExpressionKind, Parser, Scope, Statement, StatementKind, Type, TypeRef};

pub struct GraphGen {
	out: String,
	counter: i32,
}

fn format_type_ref(types: &Vec<Type>, type_ref: &TypeRef) -> String {
	let ty = &types[type_ref.id];
	format!("{}{}", match &ty {
		Type::Pointer(inner) => format!("{}*", format_type_ref(types, inner)),
		_ => ty.name().unwrap()
	}, if type_ref.reference { "&" } else { "" })
}

impl GraphGen {
	fn next_id(&mut self) -> i32 {
		let b = self.counter;
		self.counter += 1;
		b
	}

	pub fn generate_graph(parser: &Parser) -> Result<String, std::fmt::Error> {
		let mut obj = GraphGen {
			out: String::new(),
			counter: 0,
		};

		writeln!(&mut obj.out, "digraph G {{")?;
		writeln!(&mut obj.out, "node [shape=record]")?;
		writeln!(&mut obj.out, "rankdir=LR")?;
		for function in &parser.functions {
			let function = function.borrow();
			let id = obj.next_id();
			writeln!(
				&mut obj.out,
				"node{} [label=\"{}: {}\"]",
				id, function.name, format_type_ref(&parser.types.borrow(), &function.return_type)
			)?;
			obj.generate_scope(parser, &function.scope, id)?;
		}
		writeln!(&mut obj.out, "}}")?;

		Ok(obj.out)
	}

	fn generate_scope(&mut self, parser: &Parser, scope: &Scope, parent_id: i32) -> Result<(), std::fmt::Error> {
		for stmt in &scope.statements {
			let id = self.generate_statement(parser, &stmt.borrow())?;
			// TODO: change arrow shape for parent -> statement connections
			writeln!(&mut self.out, "node{} -> node{} [color=red]", parent_id, id)?;
		}
		Ok(())
	}

	fn generate_statement(&mut self, parser: &Parser, stmt: &Statement) -> Result<i32, std::fmt::Error> {
		let id = self.next_id();
		let name = {
			let x = format!("{:?}", stmt.kind);
			// incredible
			x.split_once('(').map_or(x.clone(), |(x, _)| x.to_string())
		};
		
		writeln!(
			&mut self.out,
			"node{} [color=coral3 label=\"{}\"]",
			id, name
		)?;
		for exp in &stmt.children {
			let child_id = self.generate_expression(parser, exp)?;
			writeln!(&mut self.out, "node{} -> node{}", id, child_id)?;
		}
		match &stmt.kind {
			StatementKind::While(ref scope) | StatementKind::If(ref scope) | StatementKind::Block(ref scope) => {
				self.generate_scope(parser, scope, id)?;
			}
			_ => {}
		}
		if let Some(else_branch) = &stmt.else_branch {
			let child_id = self.generate_statement(parser, else_branch)?;
			writeln!(&mut self.out, "node{} -> node{} [label=\"else\" color=red]", id, child_id)?;
		}
		Ok(id)
	}

	fn generate_expression(&mut self, parser: &Parser, exp: &Expression) -> Result<i32, std::fmt::Error> {
		let id = self.next_id();
		write!(&mut self.out, "node{} [color=darkgreen label=\"", id)?;
		let types = &parser.types.borrow();
		match &exp.kind {
			ExpressionKind::Declaration(var) => {
				write!(&mut self.out, "Declaration({}, {})", var.name, format_type_ref(types, &var.ty))?;
			}
			ExpressionKind::Variable(name) => {
				write!(&mut self.out, "Variable({name})")?;
			}
			ExpressionKind::Call(name) => {
				write!(&mut self.out, "Call({name})")?;
			}
			kind => {
				write!(&mut self.out, "{:?}", kind)?;
			}
		}
		writeln!(&mut self.out, " | {}\"]", format_type_ref(types, &exp.value_type))?;
		for child in &exp.children {
			let child_id = self.generate_expression(parser, child)?;
			writeln!(&mut self.out, "node{} -> node{}", id, child_id)?;
		}
		Ok(id)
	}
}
