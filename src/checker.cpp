#include "checker.hpp"
#include "format.hpp"
#include "enums.hpp"

auto& operator<<(std::ostream& stream, const Type& type) {
	return stream << "Type(" << type.name << ", ref=" << type.reference << ")";
}

TypeChecker::TypeChecker(Parser& parser) : m_parser(parser) {

}

void TypeChecker::check() {
	for (auto& function : m_parser.m_functions) {
		check_function(function);
	}
}

void TypeChecker::check_function(Function& function) {
	for (auto& stmt : function.statements) {
		check_statement(stmt, function);
	}
}

void TypeChecker::check_statement(Statement& stmt, Function& parent) {
	if (stmt.type == StatementType::Return) {
		if (parent.return_type.name == "void") {
			// TODO: treat void as a regular type :-)
			if (!stmt.expressions.empty())
				error_at_stmt(stmt, "return should be empty! for now..");
		} else {
			if (stmt.expressions.empty())
				error_at_stmt(stmt, "Expected expression");
			const auto type = check_expression(stmt.expressions.front(), parent, parent.return_type);
			// TODO: wrap proper reference checking
			// if ret type is ref then type must be ref
			// else then if type is ref or not doesnt matter
			if (parent.return_type.reference ? type != parent.return_type : !type.unref_eq(parent.return_type))
				error_at_stmt(stmt, format("Type mismatch, expected {} got {}", parent.return_type, type));
		}
	} else if (stmt.type == StatementType::Expression) {
		check_expression(stmt.expressions[0], parent);
	} else if (stmt.type == StatementType::If) {
		auto& data = std::get<Statement::IfData>(stmt.data);
		const auto type = check_expression(stmt.expressions[0], parent, Type { "bool" });
		if (type != Type { "bool" })
			error_at_exp(stmt.expressions.front(), "Expected bool expression");
		// TODO: proper scopes
		for (auto& child : data.children)
			check_statement(child, parent);
	} else {
		error_at_stmt(stmt, format("what the heck {}", stmt.type));
	}
}

Type TypeChecker::check_expression(Expression& expression, Function& parent, const std::optional<Type> infer_type) {
	if (expression.type == ExpressionType::Literal) {
		const auto& data = std::get<Expression::LiteralData>(expression.data);
		// TODO: use infer type
		if (std::holds_alternative<bool>(data.value))
			return expression.value_type = Type { .name = "bool" };
		else if (std::holds_alternative<int>(data.value))
			return expression.value_type = Type { .name = "i32" };
		else
			error_at_exp(expression, "TODO: strings");
	} else if (expression.type == ExpressionType::Operator) {
		const auto& data = std::get<Expression::OperatorData>(expression.data);
		if (data.op_type == OperatorType::Addition || data.op_type == OperatorType::Subtraction || data.op_type == OperatorType::Multiplication || data.op_type == OperatorType::Division) {
			const auto lhs_type = check_expression(expression.children[0], parent);
			const auto rhs_type = check_expression(expression.children[1], parent);
			if (!lhs_type.unref_eq(rhs_type))
				error_at_exp(expression, format("Types didnt match {} {}", lhs_type, rhs_type));
			return expression.value_type = lhs_type.remove_reference();
		} else if (data.op_type == OperatorType::Equals) {
			const auto lhs_type = check_expression(expression.children[0], parent);
			const auto rhs_type = check_expression(expression.children[1], parent);
			if (!lhs_type.unref_eq(rhs_type))
				error_at_exp(expression, format("Types didnt match {} {}", lhs_type, rhs_type));
			return Type { "bool" };
		} else {
			error_at_exp(expression, "Unhandled operator oops");
		}
	} else if (expression.type == ExpressionType::Call) {
		const auto& data = std::get<Expression::CallData>(expression.data);
		// TODO: better way of having builtins..
		if (data.function_name == "syscall") {
			return Type { "i32" };
		}
		const auto& funcs = m_parser.m_functions;
		const auto it = std::find_if(funcs.begin(), funcs.end(), 
			[&](const auto& function) { return function.name == data.function_name; }
		);
		if (it == funcs.end())
			error_at_exp(expression, "Unknown function");
		const auto& function = *it;
		if (function.arguments.size() != expression.children.size())
			error_at_exp(expression, "Incorrect number of arguments");
		for (size_t i = 0; i < function.arguments.size(); ++i) {
			const auto arg_type = function.arguments[i].type;
			const auto type = check_expression(expression.children[i], parent, arg_type);
			if (type != arg_type)
				error_at_exp(expression.children[i], format("Type mismatch, expected {} got {}", type, arg_type));
		}
		return function.return_type;
	} else if (expression.type == ExpressionType::Variable) {
		const auto& data = std::get<Expression::VariableData>(expression.data);
		const auto& vars = parent.scope.variables;
		const auto it = std::find_if(vars.begin(), vars.end(), [&](const auto& var) { return var.name == data.name; });
		if (it == vars.end()) {
			const auto& vars = parent.arguments;
			const auto it = std::find_if(vars.begin(), vars.end(), [&](const auto& var) { return var.name == data.name; });
			if (it == vars.end())
				error_at_exp(expression, "Unknown variable");
			return it->type.add_reference();
		} else {
			return it->type.add_reference();
		}
	} else if (expression.type == ExpressionType::Declaration) {
		const auto& data = std::get<Expression::DeclarationData>(expression.data);
		parent.scope.variables.push_back(data.var);
		return data.var.type.add_reference();
	} else if (expression.type == ExpressionType::Assignment) {
		const auto rhs_type = check_expression(expression.children[1], parent);
		const auto lhs_type = check_expression(expression.children[0], parent, rhs_type);
		if (!lhs_type.reference)
			error_at_exp(expression.children[0], "Left hand side is not a reference");
		
		if (lhs_type.name != rhs_type.name)
			error_at_exp(expression, "Both sides are not the same type");
		
		return lhs_type.remove_reference();
	} else {
		error_at_exp(expression, format("what is this {}", expression.type));
	}
}

void TypeChecker::error_at(const Span& span, const std::string_view& msg) {
	print("[error] {}", msg);
	if (!m_parser.m_file_name.empty())
		print_file_span(m_parser.m_file_name, span);
	print('\n');
	std::exit(1);
}

void TypeChecker::error_at_exp(const Expression& exp, const std::string_view& msg) {
	error_at(exp.span, msg);
}

void TypeChecker::error_at_stmt(const Statement& stmt, const std::string_view& msg) {
	error_at(stmt.span, msg);
}