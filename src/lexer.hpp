#pragma once
#include <string>
#include <fstream>
#include <vector>
#include <optional>

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

struct Span {
	size_t line = 0, column = 0;
};

struct Token {
	TokenType type;
	std::string data;

	Span span;

	Token(TokenType type) : type(type) {}
	Token(TokenType type, const std::string& data) : type(type), data(data) {}

	bool operator==(const Token& other) const {
		return type == other.type && data == other.data;
	}
};

class Lexer {
	std::istream& m_stream;
	size_t m_line = 1, m_column = 0;

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
public:
	Lexer(std::istream& stream) : m_stream(stream) {}

	std::optional<Token> get_token();
	std::vector<Token> get_tokens();
};