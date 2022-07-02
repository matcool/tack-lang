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
			// TODO: wrap proper reference checking
			// if ret type is ref then type must be ref
			// else then if type is ref or not doesnt matter
			if ((parent.return_type.reference && type != parent.return_type) || (!parent.return_type.reference && !type.unref_eq(parent.return_type)))
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
		const auto& data = std::get<Expression::LiteralData>(expression.data);
		// TODO: use infer type
		if (std::holds_alternative<bool>(data.value))
			return Type { .name = "bool" };
		else if (std::holds_alternative<int>(data.value))
			return Type { .name = "i32" };
		else
			assert(false, "TODO: string support");
	} else if (expression.type == ExpressionType::Operator) {
		const auto& data = std::get<Expression::OperatorData>(expression.data);
		if (data.op_type == OperatorType::Addition) {
			const auto lhs_type = check_expression(expression.children[0], parent);
			const auto rhs_type = check_expression(expression.children[1], parent);
			if (!lhs_type.unref_eq(rhs_type))
				assert(false, "Epic fail");
			return lhs_type.remove_reference();
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
		for (size_t i = 0; i < function.arguments.size(); ++i) {
			const auto arg_type = function.arguments[i].type;
			const auto type = check_expression(expression.children[i], parent, arg_type);
			if (type != arg_type)
				assert(false, "Type mismatch in function call,, somewhere");
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
				assert(false, "Unknown variable!");
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
		assert(lhs_type.reference, "should be reference");
		assert(lhs_type.name == rhs_type.name, "not same type...");
		
		return lhs_type.remove_reference();
	} else {
		print("what the heck is this expression?? {}\n", expression.type);
		assert(false, "poop");
		std::abort();
	}
}
