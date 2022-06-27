#include "checker.hpp"
#include "format.hpp"
#include "enums.hpp"

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
			assert(stmt.expressions.empty(), "return should be empty! for now..");
		} else {
			assert(stmt.expressions.size(), "Expected expression");
			const auto type = check_expression(stmt.expressions.front(), parent, parent.return_type);
			if (type != parent.return_type)
				assert(false, "Type did not match....");
		}
	} else if (stmt.type == StatementType::Expression) {
		check_expression(stmt.expressions[0], parent);
	} else {
		print("what the heck {}\n", stmt.type);
		assert(false, "Unknown statement type");
	}
}

Type TypeChecker::check_expression(Expression& expression, Function& parent, const std::optional<Type> infer_type) {
	if (expression.type == ExpressionType::Literal) {
		// TODO: check if its a number
		// TODO: use infer type
		return Type { .name = "i32" };
	} else if (expression.type == ExpressionType::Operator) {
		const auto& data = std::get<Expression::OperatorData>(expression.data);
		if (data.op_type == OperatorType::Addition) {
			const auto lhs_type = check_expression(expression.children[0], parent);
			const auto rhs_type = check_expression(expression.children[1], parent);
			if (lhs_type != rhs_type)
				assert(false, "Epic fail");
			return lhs_type;
		} else {
			assert(false, "not handled operator");
		}
	} else if (expression.type == ExpressionType::Call) {
		const auto& data = std::get<Expression::CallData>(expression.data);
		const auto& funcs = m_parser.m_functions;
		const auto it = std::find_if(funcs.begin(), funcs.end(), 
			[&](const auto& function) { return function.name == data.function_name; }
		);
		if (it == funcs.end())
			assert(false, "What the function??");
		const auto& function = *it;
		if (function.arguments.size() != expression.children.size())
			assert(false, "Incorrect number of arguments");
		print("gonna check call arguments!\n");
		for (size_t i = 0; i < function.arguments.size(); ++i) {
			const auto arg_type = function.arguments[i].type;
			const auto type = check_expression(expression.children[i], parent, arg_type);
			if (type != arg_type)
				assert(false, "Type mismatch in function call,, somewhere");
		}
		return function.return_type;
	} else if (expression.type == ExpressionType::Variable) {
		// TODO: keep track of variables myself,, so i can know which variables exist up until now, and also auto type
		const auto& data = std::get<Expression::VariableData>(expression.data);
		const auto& vars = parent.scope.variables;
		const auto it = std::find_if(vars.begin(), vars.end(), [&](const auto& var) { return var.name == data.name; });
		if (it == vars.end()) {
			const auto& vars = parent.arguments;
			const auto it = std::find_if(vars.begin(), vars.end(), [&](const auto& var) { return var.name == data.name; });
			if (it == vars.end())
				assert(false, "Unknown variable!");
			return it->type;
		}
		return it->type;
	} else {
		print("what the heck is this expression?? {}\n", expression.type);
		assert(false, "poop");
		std::abort();
	}
}