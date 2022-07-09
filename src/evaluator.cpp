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
	return expression.match(
		[&](const Expression::LiteralData& data) {
			return Value { expression.value_type, std::get<int>(data.value) };
		},
		[&](const Expression::DeclarationData& data) {
			Value& value = scope.add_variable(data.var.name, Value { data.var.type });
			return Value { data.var.type.add_reference(), std::ref(value) };
		},
		[&](const Expression::VariableData& data) {
			auto result = scope.get_variable(data.name);
			if (!result.has_value()) {
				assert(false, "variable not found in evaluator, should not happen");
			}
			Value& value = result.value();
			return Value { value.type.add_reference(), std::ref(value) };
		},
		[&](MatchValue<ExpressionType::Assignment>) {
			auto rhs = eval_expression(expression.children[1], parent, scope);
			auto lhs = eval_expression(expression.children[0], parent, scope);
			Value& value = std::get<std::reference_wrapper<Value>>(lhs.data);
			value.data = rhs.data;
			return lhs;
		},
		[&](auto) {
			print("{}\n", enum_name(expression.type));
			assert(false, "unhandled expression");
			return Value{};
		}
	);

}