#include <fstream>
#include <string>
#include <iostream>
#include <vector>
#include <cstdlib>
#include <string_view>
#include <algorithm>
#include "format.hpp"
#include <filesystem>
#include <unordered_map>
#include <optional>
#include <variant>
#include "main.hpp"

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
	}
	return "";
}

inline const char* enum_name(const StatementType& t) {
	switch (t) {
		case StatementType::Expression: return "Expression";
		case StatementType::Assignment: return "Assignment";
		case StatementType::Return: return "Return";
	}
	return "";
}

inline const char* enum_name(const ExpressionType& t) {
	switch (t) {
		case ExpressionType::Literal: return "Literal";
		case ExpressionType::Declaration: return "Declaration";
		case ExpressionType::Variable: return "Variable";
	}
	return "";
}

bool is_whitespace(char c) {
	return c == ' ' || c == '\t' || c == '\r' || c == '\n' || c == '\f' || c == '\v';
}

auto& operator<<(std::ostream& stream, const Token& token) {
	stream << token.line << ':' << token.column << ' ';
	stream << enum_name(token.type);
	if (!token.data.empty()) stream << " \"" << token.data << '"';
	return stream;
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
			token.line = line;
			token.column = col;
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
				if (m_stream.peek() == '=')
					assert(false, "TODO: implement ==");
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
			default: {
				// if (is_whitespace(c)) return TokenType::Unknown;
				std::string str;
				str.push_back(c);
				while (m_stream.good()) {
					const auto next = static_cast<char>(m_stream.peek());
					if (next >= 'a' && next <= 'z' || next >= 'A' && next <= 'Z' || next >= '0' && next <= '9') {
						str.push_back(next);
						next_char(next);
						m_stream.get(c);
					} else
						break;
				}
				if (str == "fn" || str == "let" || str == "return")
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
		auto token = get_token();
		if (token) {
			print("got token {}\n", *token);
			output.push_back(*token);
		}
	}
	return output;
}

Parser::Parser(const std::string_view& file_name) : m_file_name(file_name) {

}

void Parser::error_at_token(const Token& token, const std::string_view& msg) {
	print("[error] {}", msg);
	if (!m_file_name.empty() && token.line) {
		print(" @ {}:{}:{}\n", m_file_name, token.line, token.column);
		std::ifstream file(m_file_name);
		std::string line;
		size_t counter = 1;
		while (std::getline(file, line)) {
			if (counter == token.line)
				break;
			++counter;
		}
		print("{}\n", line);
		for (size_t i = 1; i < token.column; ++i)
			print(' ');
		print("^ here\n");
	} else
		print('\n');
	std::abort();
}

Token& Parser::expect_token_type(Token& token, TokenType type, const std::string_view& msg) {
	if (token.type != type)
		error_at_token(token, msg);
	return token;
}

void Parser::parse(ArrayView<Token> tokens) {
	ArrayStream stream(tokens);
	while (stream.size()) {
		auto& token = stream.get();
		if (token.type == TokenType::Keyword && token.data == "fn") {
			auto& function = m_functions.emplace_back();
			function.name = expect_token_type(stream.get(), TokenType::Identifier, "Expected function name").data;
			expect_token_type(stream.get(), TokenType::LeftParen, "Expected function args");
			// auto rb = stream.view().find([](Token& token) { return token.type == TokenType::RightParen; });
			// if (!rb)
			// 	error_at_token(stream.peek(), "Unclosed parenthesis");
			
			// ArrayStream arg_list_tokens(tokens.slice(i + 1, i + *rb));
			while (stream.peek().type != TokenType::RightParen) {
				function.arguments.push_back(parse_var_decl(stream));
				auto& next = stream.peek();
				if (next.type != TokenType::Comma && next.type != TokenType::RightParen)
					error_at_token(next, "Expected comma");
			}
			stream.get();

			// TODO: default to void
			expect_token_type(stream.get(), TokenType::TypeIndicator, "Expected type indicator");

			// TODO: properly parse types
			auto type_token = expect_token_type(stream.get(), TokenType::Identifier, "Expected type");
			function.return_type = Type { type_token.data };
			
			expect_token_type(stream.get(), TokenType::LeftBracket, "Expected bracket");

			parse_function(function, stream);
		} else {
			assert(false, "Unimplemented token outside in global scope");
		}
	}
}

Variable Parser::parse_var_decl(ArrayStream<Token>& tokens) {
	auto name_token = tokens.get();
	if (name_token.type != TokenType::Identifier)
		error_at_token(name_token, "Expected variable name");
	if (tokens.get().type != TokenType::TypeIndicator)
		error_at_token(tokens.prev(), "Expected type indicator");
	// TODO: multi token type
	auto type_token = tokens.get();
	if (type_token.type != TokenType::Identifier)
		error_at_token(type_token, "Expected type");

	return Variable { type_token.data, name_token.data };
}

void Parser::parse_function(Function& function, ArrayStream<Token>& tokens) {
	auto start = tokens.pos();
	while (tokens.peek().type != TokenType::RightBracket) {
		if (tokens.get().type == TokenType::Semicolon) {
			auto i = tokens.pos();
			tokens.seek_to(0);
			function.statements.push_back(parse_statement(tokens.view().slice(start, i), &function));
			start = i;
			tokens.seek_to(start);
		}
	}
	tokens.get();
}

Statement Parser::parse_statement(ArrayView<Token> tokens, Function* parent) {
	if (tokens.size() == 0)
		assert(false, "Empty statements are unimplemented");
	auto& first = tokens[0];
	if (first.type == TokenType::Keyword && first.data == "return") {
		assert(parent, "Return statement cannot appear outside function");
		auto i = tokens.find([](Token& token) { return token.type == TokenType::Semicolon; });
		assert(i.has_value(), "No semicolon found after return");
		return Statement {
			StatementType::Return,
			{ parse_expression(tokens.slice(1, *i), parent->return_type) }
		};
	} else if (auto pos = tokens.find([](Token& token) { return token.type == TokenType::Assign; }); pos) {
		
	} else {
		assert(false, "Fuck!");
	}
}

Expression Parser::parse_expression(ArrayView<Token> tokens, const Type infer_type) {
	assert(tokens.size() == 1, "im silly");
	assert(tokens[0].type == TokenType::Number, "im silly");
	Expression exp(ExpressionType::Literal);
	exp.prefer_type = infer_type;
	exp.data = Expression::LiteralData { std::stoi(tokens[0].data) };
	return exp;
}

int main(int argc, char** argv) {
	auto args = ArrayView<char*>(argv, argc);
	if (args.size() < 2) {
		print(
			"Silly compiler for a silly language.\n\n"

			"Usage: {} input [output]\n\n"
			
			"  input - input file to compile\n"
			"  output - output llvm ir file. if not provided will use input but with the extension replaced with .ll\n",
			args[0]
		);
		return 1;
	}

	auto input_file = std::ifstream(args[1]);
	const std::filesystem::path output_path = args.size() < 3 ? std::filesystem::path(args[1]).replace_extension(".ll") : args[2];
	if (args.size() < 3 && std::filesystem::exists(output_path)) {
		print("File {} already exists. To overwrite manually provide it as the output\n", output_path);
		return 1;
	}

	Lexer lexer(input_file);
	auto tokens = lexer.get_tokens();
	print("File tokenized\n");
	for (auto& token : tokens) {
		print(" - {}\n", token);
	}

	input_file.close();

	Parser parser(args[1]);
	parser.parse(tokens);
	print("File parsed\n");

	for (auto& function : parser.m_functions) {
		print("Function {} -> {}\n", function.name, function.return_type.name);
		for (auto& statement : function.statements) {
			print("  {}\n", enum_name(statement.type));
			for (auto& expression : statement.expressions) {
				print("    {}\n", enum_name(expression.type));
			}
		}
	}

	return 0;
}