#include "evaluator.hpp"
#include "enums.hpp"

int Evaluator::run() {
	for (auto& function : m_parser.m_functions) {
		if (function.name == "main") {
			const auto value = eval_function(function);
			return std::get<int>(value.data);
		}
	}
	assert(false, "main not found");
	return -1;
}

Evaluator::Value Evaluator::eval_function(Function& function) {
	Scope scope;
	for (auto& stmt : function.statements) {
		if (stmt.type == StatementType::Return) {
			return eval_expression(stmt.expressions[0], function, scope);
		} else if (stmt.type == StatementType::Expression) {
			eval_expression(stmt.expressions[0], function, scope);
		}
	}
}

Evaluator::Value Evaluator::eval_expression(Expression& expression, Function& parent, Scope& scope) {
	// returns referenced value if its a ref, otherwise return the value
	const auto get_value_or_ref = [](Value& value) -> Value& {
		if (value.type.reference) {
			return std::get<std::reference_wrapper<Value>>(value.data);
		} else {
			return value;
		}
	};
	if (expression.type == ExpressionType::Literal) {
		const auto& data = std::get<Expression::LiteralData>(expression.data);
		return Value { expression.value_type, std::get<int>(data.value) };
	} else if (expression.type == ExpressionType::Operator) {
		const auto& data = std::get<Expression::OperatorData>(expression.data);
		if (data.op_type == OperatorType::Addition) {
			auto lhs = eval_expression(expression.children[0], parent, scope);
			auto rhs = eval_expression(expression.children[1], parent, scope);
			if (expression.value_type.name == "i32") {
				return Value {
					expression.value_type,
					// urm um yeah yup
					std::get<int>(get_value_or_ref(lhs).data) + std::get<int>(get_value_or_ref(rhs).data)
				};
			} else {
				assert(false, "unhandled type addition");
			}
		}
	} else if (expression.type == ExpressionType::Declaration) {
		const auto& data = std::get<Expression::DeclarationData>(expression.data);
		Value& value = scope.add_variable(data.var.name, Value { data.var.type });
		return Value { data.var.type.add_reference(), std::ref(value) };
	} else if (expression.type == ExpressionType::Variable) {
		const auto& data = std::get<Expression::VariableData>(expression.data);
		auto result = scope.get_variable(data.name);
		if (!result.has_value()) {
			assert(false, "variable not found in evaluator, should not happen");
		}
		Value& value = result.value();
		return Value { value.type.add_reference(), std::ref(value) };
	} else if (expression.type == ExpressionType::Assignment) {
		auto rhs = eval_expression(expression.children[1], parent, scope);
		auto lhs = eval_expression(expression.children[0], parent, scope);
		Value& value = std::get<std::reference_wrapper<Value>>(lhs.data);
		value.data = rhs.data;
		return lhs;
	}
	print("{}\n", enum_name(expression.type));
	assert(false, "unhandled expression");
}