#include "lexer.hpp"
#include "utils.hpp"
#include "enums.hpp"

bool is_whitespace(char c) {
	return c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\f' || c == '\v';
}

void Lexer::eat_until(std::string& buffer, char target) {
	while (m_stream.good()) {
		char c;
		m_stream.get(c);
		next_char(c);
		if (c == target) return;
		buffer.push_back(c);
	}
}

std::optional<Token> Lexer::get_token() {
	char c;
	while (m_stream.good()) {
		m_stream.get(c);
		if (m_stream.eof()) return {};
		next_char(c);
		const auto line = m_line;
		const auto col = m_column;
		const auto ret = [&](Token token) {
			token.span.line = line;
			token.span.column = col;
			return token;
		};
		switch (c) {
			case '\n':
			case '\t':
			case '\r':
			case ' ':
				continue;
			case ';': return ret(TokenType::Semicolon);
			case '"': {
				std::string str;
				eat_until(str, '"');
				return ret(Token(TokenType::String, str));
			}
			case ',': return ret(TokenType::Comma);
			case '=': {
				if (m_stream.peek() == '=') {
					m_stream.get(c);
					return ret(Token(TokenType::Operator, "=="));
				}
				return ret(TokenType::Assign);
			}
			case '(': return ret(TokenType::LeftParen);
			case ')': return ret(TokenType::RightParen);
			case '{': return ret(TokenType::LeftBracket);
			case '}': return ret(TokenType::RightBracket);
			case ':': return ret(TokenType::TypeIndicator);
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9': {
				std::string str;
				str.push_back(c);
				while (m_stream.good()) {
					const auto next = static_cast<char>(m_stream.peek());
					if (next < '0' || next > '9') break;
					else {
						m_stream.get(c);
						str.push_back(c);
						next_char(c);
					}
				}

				return ret(Token(TokenType::Number, str));
			}
			case '-': return ret(Token(TokenType::Operator, "-"));
			case '~': return ret(Token(TokenType::Operator, "~"));
			case '+': return ret(Token(TokenType::Operator, "+"));
			case '*': return ret(Token(TokenType::Operator, "*"));
			case '!': {
				if (m_stream.peek() == '=') {
					m_stream.get();
					return ret(Token(TokenType::Operator, "!="));
				} else {
					return ret(Token(TokenType::Operator, "!"));
				}
			}
			case '/': {
				if (m_stream.peek() == '/') {
					std::string comment;
					eat_until(comment, '\n');
					return {};
				} else
					return ret(Token(TokenType::Operator, "/"));
			}
			default: {
				// if (is_whitespace(c)) return TokenType::Unknown;
				std::string str;
				str.push_back(c);
				while (m_stream.good()) {
					const auto next = static_cast<char>(m_stream.peek());
					if ((next >= 'a' && next <= 'z') || (next >= 'A' && next <= 'Z') || (next >= '0' && next <= '9')) {
						str.push_back(next);
						next_char(next);
						m_stream.get(c);
					} else
						break;
				}
				// TODO: clean this up
				if (str == "fn" || str == "let" || str == "return" || str == "true" || str == "false" || str == "if" || str == "while")
					return ret(Token(TokenType::Keyword, str));
				else
					return ret(Token(TokenType::Identifier, str));
			}
		}
	}
	return {};
}

std::vector<Token> Lexer::get_tokens() {
	std::vector<Token> output;
	while (m_stream.good()) {
		const auto token = get_token();
		if (token) {
			output.push_back(*token);
		}
	}
	return output;
}
