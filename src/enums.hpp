#pragma once
#include "lexer.hpp"
#include "parser.hpp"

inline const char* enum_name(const TokenType& t) {
	switch (t) {
		case TokenType::Unknown: return "Unknown";
		case TokenType::Semicolon: return "Semicolon";
		case TokenType::String: return "String";
		case TokenType::LeftParen: return "LeftParen";
		case TokenType::RightParen: return "RightParen";
		case TokenType::TypeIndicator: return "TypeIndicator";
		case TokenType::Number: return "Number";
		case TokenType::Identifier: return "Identifier";
		case TokenType::LeftBracket: return "LeftBracket";
		case TokenType::RightBracket: return "RightBracket";
		case TokenType::Assign: return "Assign";
		case TokenType::Comma: return "Comma";
		case TokenType::Keyword: return "Keyword";
		case TokenType::Operator: return "Operator";
	}
	return "";
}

inline const char* enum_name(const StatementType& t) {
	switch (t) {
		case StatementType::Expression: return "Expression";
		case StatementType::Return: return "Return";
		case StatementType::If: return "If";
	}
	return "";
}

inline const char* enum_name(const ExpressionType& t) {
	switch (t) {
		case ExpressionType::Literal: return "Literal";
		case ExpressionType::Declaration: return "Declaration";
		case ExpressionType::Variable: return "Variable";
		case ExpressionType::Assignment: return "Assignment";
		case ExpressionType::Operator: return "Operator";
		case ExpressionType::Call: return "Call";
	}
	return "";
}

inline const char* enum_name(const OperatorType& t) {
	switch (t) {
		case OperatorType::Negation: return "Negation";
		case OperatorType::Not: return "Not";
		case OperatorType::Bitflip: return "Bitflip";
		case OperatorType::Addition: return "Addition";
		case OperatorType::Subtraction: return "Subtraction";
		case OperatorType::Multiplication: return "Multiplication";
		case OperatorType::Division: return "Division";
	}
	return "";
}

template <class Enum>
requires requires(const Enum value) { enum_name(value); }
auto& operator<<(std::ostream& stream, const Enum value) {
	return stream << enum_name(value);
}
