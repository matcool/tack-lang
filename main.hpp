#pragma once
#include <optional>
#include <vector>
#include "format.hpp"
#include "util.hpp"

enum class TokenType {
	Unknown,
	Semicolon,
	String,
	LeftParen,
	RightParen,
	LeftBracket,
	RightBracket,
	TypeIndicator, // :
	Number,
	Identifier,
	Assign,
	Comma,
	Keyword,
	Operator,
};

struct Token {
	TokenType type;
	std::string data;

	size_t line = 0, column = 0;
	// TODO: store origin (file, line, column)

	Token(TokenType type) : type(type) {}
	Token(TokenType type, const std::string& data) : type(type), data(data) {}
};

class Lexer {
public:
	std::istream& m_stream;
	size_t m_line = 1, m_column = 0;

	Lexer(std::istream& stream) : m_stream(stream) {}

	inline void new_line() { ++m_line; m_column = 0; }
	inline void next_char(char c) {
		if (c == '\n')
			new_line();
		else if (c == '\t')
			m_column += 4; // sorry for using the correct size
		else
			++m_column;
	}
	void eat_until(std::string& buffer, char target);
	std::optional<Token> get_token();
	std::vector<Token> get_tokens();
};

struct Type {
	// TODO: enum
	std::string name;
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
		std::variant<int, std::string> value;
	};
	struct OperatorData {
		OperatorType op_type;
	};
	struct CallData {
		// TODO: have the function name be an expression?
		std::string function_name;
	};
	std::variant<bool, DeclarationData, VariableData, LiteralData, OperatorData, CallData> data;
	Expression(const ExpressionType type) : type(type) {}
	template <class T>
	Expression(const ExpressionType type, T&& data, const std::vector<Expression> children) : 
		type(type), children(children), data(std::forward<T>(data)) {}
};

enum class StatementType {
	Expression, // statement is just an expression
	Return,     // statement returns an expression
};

struct Statement {
	StatementType type;
	std::vector<Expression> expressions;
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

	Type parse_type();

	// impl based on https://en.wikipedia.org/wiki/Recursive_descent_parser
	// expression = stage1 ["=" expression]
	Expression parse_expression();
	// stage1 = stage2 ["+"|"-" stage1]
	Expression parse_exp_stage1();
	// stage2 = primary ["/"|"*" stage2]
	Expression parse_exp_stage2();
	// primary = identifier | literal | unary primary | "(" expression ")"
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

class Compiler {
public:
	std::ostream& m_stream;
	Parser& m_parser;
	Function* m_cur_function = nullptr;
	// TODO: not
	size_t m_var_counter = 0;
	std::unordered_map<std::string, int> m_variables;

	Compiler(std::ostream& output, Parser& parser) : m_stream(output), m_parser(parser) {}

	template <class... Args>
	void write(const std::string_view& format, Args&&... args) {
		format_to(m_stream, format, args...);
		m_stream << '\n';
	}

	void compile_expression(Expression&, bool ref = false);
	void compile_statement(Statement&);
	void compile_function(Function&);

	void compile();

};

void print_expression(const Expression& exp, const int depth = 0);