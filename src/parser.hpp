#pragma once
#include <string>
#include <vector>
// TODO: get rid of this >:)
#include <variant>
#include "utils.hpp"
#include "lexer.hpp"

struct Type {
	// TODO: enum for built in types, etc
	std::string name;
	bool reference = false;

	bool operator==(const Type&) const = default;
	Type add_reference() const {
		auto result = *this;
		result.reference = true;
		return result;
	}
	Type remove_reference() const {
		auto result = *this;
		result.reference = false;
		return result;
	}
	bool unref_eq(const Type& other) const {
		return name == other.name;
	}
};

struct Variable {
	Type type;
	std::string name;
};

enum class ExpressionType {
	Literal,
	Declaration,
	Variable,
	Assignment,
	Operator,
	Call,
};

enum class OperatorType {
	// Unary
	Negation,
	Not,
	Bitflip,
	// Binary
	Addition,
	Subtraction,
	Multiplication,
	Division,
	Equals,
	NotEquals,
};

struct Expression {
	ExpressionType type;
	// TODO: have an specific value for this, instead of just void
	Type prefer_type = Type { "void" };
	std::vector<Expression> children;
	// TODO: consider dynamic polymorphism instead of this
	struct DeclarationData {
		Variable var;
	};
	struct VariableData {
		std::string name;
	};
	struct LiteralData {
		std::variant<int, bool, std::string> value;
	};
	struct OperatorData {
		OperatorType op_type;
	};
	struct CallData {
		// TODO: have the function name be an expression?
		std::string function_name;
	};
	std::variant<std::monostate, DeclarationData, VariableData, LiteralData, OperatorData, CallData> data;
	Span span;

	Expression(const ExpressionType type) : type(type) {}
	template <class T>
	Expression(const ExpressionType type, T&& data, const std::vector<Expression> children) : 
		type(type), children(children), data(std::forward<T>(data)) {}
};

enum class StatementType {
	Expression, // statement is just an expression
	Return,     // statement returns an expression
	If,
};

struct Statement {
	StatementType type;
	std::vector<Expression> expressions;
	Span span;
	struct IfData {
		// TODO: make this a scope, that is if they hold statements
		std::vector<Statement> children;
	};
	std::variant<std::monostate, IfData> data;
};

struct Function;

struct Scope {
	std::vector<Variable> variables;
};

struct Function {
	Type return_type;
	std::string name;
	std::vector<Variable> arguments;
	Scope scope;
	std::vector<Statement> statements;
};

class Parser {
public:
	Scope m_global_scope;
	std::vector<Function> m_functions;
	std::string m_file_name;
	ArrayStream<Token> m_tokens;
	Function* m_cur_function = nullptr;

	// Parser() {}
	Parser(const std::string_view& file_name, ArrayStream<Token> tokens);

	Variable parse_var_decl();
	void parse_function(Function& function);
	Statement parse_statement();

	std::vector<Statement> parse_block();

	Type parse_type();

	Expression parse_expression();
	Expression parse_exp_inner(int prio);
	Expression parse_exp_primary();

	void error_at_token(const Token& token, const std::string_view& msg);
	Token& expect_token_type(Token& token, TokenType type, const std::string_view& msg);
	
	// Parses comma list enclosed by parenthesis (todo: customizable)
	// callable is expected to eat the tokens for each thing in the list
	// leaving either the comma or right paren after
	template <class Func>
	void parse_comma_list(Func&& callable) {
		while (true) {
			if (m_tokens.peek().type == TokenType::RightParen) {
				m_tokens.get();
				return;
			}

			callable();

			const auto& next = m_tokens.peek();
			if (next.type == TokenType::Comma) {
				m_tokens.get();
			} else if (next.type == TokenType::RightParen) {
				m_tokens.get();
				return;
			} else {
				error_at_token(next, "Unexpected token");
			}
		}
	}

	void parse();
};