use crate::ast::{Expression, ExpressionKind, Function, Scope, Statement, StatementKind, AST};

pub fn dump(ast: &AST) {
	for func in &ast.functions {
		dump_function(ast, func);
	}
}

fn dump_function(ast: &AST, function: &Function) {
	println!("Function: {}", function.name);
	println!("Args:");
	for arg in &function.arguments {
		println!("  {}: {}", arg.name, arg.ty.formatted(ast));
	}
	println!("Returns: {}", function.return_type.formatted(ast));
	println!("Scope:");
	dump_scope(ast, &function.scope, "  ");
}

fn dump_scope(ast: &AST, scope: &Scope, indent: &str) {
	for stmt in scope.statements.borrow().iter() {
		// println!("{indent}{}: {}", var.name, var.ty.formatted(ast));
		dump_statement(ast, stmt, indent);
	}
}

fn dump_statement(ast: &AST, stmt: &Statement, indent: &str) {
	match &stmt.kind {
		StatementKind::Expression => {
			let expr = stmt.children.first().unwrap();
			dump_expression(ast, expr, indent);
		}
		StatementKind::If(body, else_stmt) => {
			println!("{indent}Statement: If");
			let indent = format!("  {indent}");
			println!("{indent}Condition:");
			dump_expression(ast, &stmt.children[0], &format!("  {indent}"));
			println!("{indent}Body:");
			dump_scope(ast, body, &format!("  {indent}"));
			if let Some(else_stmt) = else_stmt {
				println!("{indent}Else:");
				dump_statement(ast, else_stmt, &format!("  {indent}"));
			}
		}
		StatementKind::While(body) => {
			println!("{indent}Statement: While");
			let indent = format!("  {indent}");
			println!("{indent}Condition:");
			dump_expression(ast, &stmt.children[0], &format!("  {indent}"));
			println!("{indent}Body:");
			dump_scope(ast, body, &format!("  {indent}"));
		}
		StatementKind::Block(scope) => {
			println!("{indent}Scope:");
			dump_scope(ast, scope, &format!("  {indent}"));
		}
		_ => {
			let name = format!("{:?}", stmt.kind);
			println!("{indent}Statement: {name}");
			for expr in &stmt.children {
				dump_expression(ast, expr, &format!("  {indent}"));
			}
		}
	}
	// let mut new_indent = format!("  {indent}");
	// let name = match &stmt.kind {
	// 	StatementKind::Expression => {
	// 		let expr = stmt.children.first().unwrap();
	// 		dump_expression(ast, expr, indent);
	// 		return;
	// 	}
	// 	StatementKind::If(, ) => {}
	// 	k => format!("{:?}", k),
	// };
	// println!("{indent}Statement: {name}");
	// for expr in &stmt.children {
	// 	dump_expression(ast, expr, &new_indent);
	// }
}

fn dump_expression(ast: &AST, expr: &Expression, indent: &str) {
	let name = match &expr.kind {
		ExpressionKind::Declaration(var) => {
			format!(
				"Declaration({} ({}): {})",
				var.name,
				var.unique_id,
				var.ty.formatted(ast)
			)
		}
		ExpressionKind::Identifier(var) => {
			format!(
				"Identifier({} ({}): {})",
				var.name,
				var.unique_id,
				var.ty.formatted(ast)
			)
		}
		k => format!("{:?}", k),
	};
	println!(
		"{indent}Expression: {name} -> {}",
		expr.value_type.formatted(ast)
	);
	for expr in &expr.children {
		dump_expression(ast, expr, &format!("  {indent}"));
	}
}
