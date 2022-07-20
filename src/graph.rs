use std::fmt::Write;

use crate::parser::{Expression, ExpressionKind, Parser, Scope, Statement};

pub struct GraphGen {
	out: String,
	counter: i32,
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
			let id = obj.next_id();
			writeln!(
				&mut obj.out,
				"node{} [label=\"{}: {}\"]",
				id, function.name, function.return_type
			)?;
			obj.generate_scope(&function.scope, id)?;
		}
		writeln!(&mut obj.out, "}}")?;

		Ok(obj.out)
	}

	fn generate_scope(&mut self, scope: &Scope, parent_id: i32) -> Result<(), std::fmt::Error> {
		for stmt in &scope.statements {
			let id = self.generate_statement(&stmt.borrow())?;
			writeln!(&mut self.out, "node{} -> node{}", parent_id, id)?;
		}
		Ok(())
	}

	fn generate_statement(&mut self, stmt: &Statement) -> Result<i32, std::fmt::Error> {
		let id = self.next_id();
		writeln!(
			&mut self.out,
			"node{} [color=coral3 label=\"{:?}\"]",
			id, stmt.kind
		)?;
		for exp in &stmt.children {
			let child_id = self.generate_expression(exp)?;
			writeln!(&mut self.out, "node{} -> node{}", id, child_id)?;
		}
		Ok(id)
	}

	fn generate_expression(&mut self, exp: &Expression) -> Result<i32, std::fmt::Error> {
		let id = self.next_id();
		write!(&mut self.out, "node{} [color=darkgreen label=\"", id)?;
		match &exp.kind {
			ExpressionKind::Declaration(var) => {
				write!(&mut self.out, "Declaration({}, {})", var.name, var.ty)?;
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
		writeln!(&mut self.out, " | {}\"]", exp.value_type)?;
		for child in &exp.children {
			let child_id = self.generate_expression(child)?;
			writeln!(&mut self.out, "node{} -> node{}", id, child_id)?;
		}
		Ok(id)
	}
}
