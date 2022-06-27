#pragma once
#include "parser.hpp"

class TypeChecker {
	Parser& m_parser;
public:
	TypeChecker(Parser& parser);

	void check();

	void check_function(Function& function);
	void check_statement(Statement& stmt, Function& parent);
	// TODO: use scopes instead of Function..
	Type check_expression(Expression& expr, Function& parent, const std::optional<Type> infer_type = std::nullopt);
};