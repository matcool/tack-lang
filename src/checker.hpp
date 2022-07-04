#pragma once
#include "parser.hpp"

class TypeChecker {
	Parser& m_parser;
	
	[[noreturn]] void error_at_exp(const Expression& token, const std::string_view& msg);
	[[noreturn]] void error_at_stmt(const Statement& stmt, const std::string_view& msg);
	[[noreturn]] void error_at(const Span& span, const std::string_view& msg);
public:
	TypeChecker(Parser& parser);

	void check();

	void check_function(Function& function);
	void check_statement(Statement& stmt, Function& parent);
	// TODO: use scopes instead of Function..
	Type check_expression(Expression& expr, Function& parent, const std::optional<Type> infer_type = std::nullopt);
};