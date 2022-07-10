#pragma once

#include "parser.hpp"

class Evaluator {
	Parser& m_parser;

	struct Value {
		Type type;
		std::variant<std::monostate, int, bool, std::reference_wrapper<Value>> data;
	};
	struct Scope {
		std::vector<std::pair<std::string, Value>> variables;

		std::optional<std::reference_wrapper<Value>> get_variable(const std::string& name) {
			// tfw no insertion order sorted map
			for (auto& [var_name, value] : variables) {
				if (var_name == name) {
					return value;
				}
			}
			return std::nullopt;
		}
		Value& add_variable(const std::string& name, Value&& value) {
			variables.push_back({name, value});
			return variables.back().second;
		}
	};
	Value eval_function(Function& function, std::vector<Value> args);
	Value eval_expression(Expression&, Function& parent, Scope& scope);
public:
	Evaluator(Parser& parser) : m_parser(parser) {}

	int run();
};
