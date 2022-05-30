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
		case TokenType::Operator: return "Operator";
	}
	return "";
}

inline const char* enum_name(const StatementType& t) {
	switch (t) {
		case StatementType::Expression: return "Expression";
		case StatementType::Return: return "Return";
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
			case '-': return ret(Token(TokenType::Operator, "-"));
			case '~': return ret(Token(TokenType::Operator, "~"));
			case '!': return ret(Token(TokenType::Operator, "!"));
			case '+': return ret(Token(TokenType::Operator, "+"));
			case '*': return ret(Token(TokenType::Operator, "*"));
			case '/': {
				if (static_cast<char>(m_stream.peek()) == '/') {
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
			output.push_back(*token);
		}
	}
	return output;
}

Parser::Parser(const std::string_view& file_name, ArrayStream<Token> tokens)
	: m_file_name(file_name),
	m_tokens(tokens) {}

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
		for (const auto c : line) {
			if (c == '\t')
				print("    ");
			else
				print("{}", c);
		}
		print("\n");
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

void Parser::parse() {
	while (m_tokens.size()) {
		auto& token = m_tokens.get();
		if (token.type == TokenType::Keyword && token.data == "fn") {
			auto& function = m_functions.emplace_back();
			function.name = expect_token_type(m_tokens.get(), TokenType::Identifier, "Expected function name").data;
			expect_token_type(m_tokens.get(), TokenType::LeftParen, "Expected function args");
			// auto rb = m_tokens.view().find([](Token& token) { return token.type == TokenType::RightParen; });
			// if (!rb)
			// 	error_at_token(m_tokens.peek(), "Unclosed parenthesis");
			
			// Arraym_tokens arg_list_tokens(tokens.slice(i + 1, i + *rb));
			while (m_tokens.peek().type != TokenType::RightParen) {
				function.arguments.push_back(parse_var_decl());
				auto& next = m_tokens.peek();
				if (next.type != TokenType::Comma && next.type != TokenType::RightParen)
					error_at_token(next, "Expected comma");
			}
			m_tokens.get();

			// TODO: default to void
			expect_token_type(m_tokens.get(), TokenType::TypeIndicator, "Expected type indicator");

			// TODO: properly parse types
			auto type_token = expect_token_type(m_tokens.get(), TokenType::Identifier, "Expected type");
			function.return_type = Type { type_token.data };
			
			expect_token_type(m_tokens.get(), TokenType::LeftBracket, "Expected bracket");

			parse_function(function);
		} else {
			assert(false, "Unimplemented token outside in global scope");
		}
	}
}

Variable Parser::parse_var_decl() {
	auto name_token = m_tokens.get();
	if (name_token.type != TokenType::Identifier)
		error_at_token(name_token, "Expected variable name");
	if (m_tokens.get().type != TokenType::TypeIndicator)
		error_at_token(m_tokens.prev(), "Expected type indicator");
	// TODO: multi token type
	auto type_token = m_tokens.get();
	if (type_token.type != TokenType::Identifier)
		error_at_token(type_token, "Expected type");

	return Variable { type_token.data, name_token.data };
}

void Parser::parse_function(Function& function) {
	m_cur_function = &function;
	while (m_tokens.peek().type != TokenType::RightBracket) {
		function.statements.push_back(parse_statement());
		expect_token_type(m_tokens.get(), TokenType::Semicolon, "Expected semicolon");
	}
	m_tokens.get();
	m_cur_function = nullptr;
}

Statement Parser::parse_statement() {
	auto& first = m_tokens.peek();
	if (first.type == TokenType::Keyword && first.data == "return") {
		m_tokens.get();
		assert(m_cur_function, "Return statement cannot appear outside function");
		return Statement {
			StatementType::Return,
			{ parse_expression() }
		};
	} else {
		const auto exp = parse_expression();
		return Statement {
			StatementType::Expression,
			{ exp }
		};
	}
}

OperatorType op_type_from_token(const Token& token) {
	assert(token.type == TokenType::Operator, "token should be an operator");
	if (token.data == "+") return OperatorType::Addition;
	if (token.data == "-") return OperatorType::Subtraction;
	if (token.data == "*") return OperatorType::Multiplication;
	if (token.data == "/") return OperatorType::Division;
	if (token.data == "!") return OperatorType::Not;
	if (token.data == "~") return OperatorType::Bitflip;
	return {};
}

Expression Parser::parse_exp_primary() {
	const auto& token = m_tokens.get();
	if (token.type == TokenType::Number) {
		Expression exp(ExpressionType::Literal);
		exp.data = Expression::LiteralData { std::stoi(token.data) };
		return exp;
	} else if (token.type == TokenType::Identifier) {
		Expression exp(ExpressionType::Variable);
		exp.data = Expression::VariableData { token.data };
		return exp;
	} else if (token.type == TokenType::LeftParen) {
		const auto exp = parse_expression();
		expect_token_type(m_tokens.get(), TokenType::RightParen, "Expected )");
		return exp;
	} else if (token.type == TokenType::Operator) {
		auto type = op_type_from_token(token);
		if (type == OperatorType::Subtraction) type = OperatorType::Negation;

		if (type != OperatorType::Bitflip
		 && type != OperatorType::Negation
		 && type != OperatorType::Not)
			error_at_token(token, "Invalid unary operator");

		Expression exp(ExpressionType::Operator);
		exp.data = Expression::OperatorData { type };
		exp.children.push_back(parse_exp_primary());
		return exp;
	} else if (token.type == TokenType::Keyword && token.data == "let") {
		const auto var = parse_var_decl();
		m_cur_function->scope.variables.push_back(var);
		return Expression {
			ExpressionType::Declaration,
			Expression::DeclarationData { var },
			{}
		};
	} else {
		error_at_token(token, "Tried to parse unknown primary expression");
	}
	std::abort();
}

Expression Parser::parse_exp_stage2() {
	const auto factor = parse_exp_primary();
	const auto& next = m_tokens.peek();
	if (next.type == TokenType::Operator
		&& (next.data == "/" || next.data == "*")) {
		Expression exp(ExpressionType::Operator);
		exp.data = Expression::OperatorData { op_type_from_token(m_tokens.get()) };
		exp.children.push_back(factor);
		exp.children.push_back(parse_exp_stage2());
		return exp;
	} else {
		return factor;
	}
}

Expression Parser::parse_exp_stage1() {
	const auto term = parse_exp_stage2();
	const auto& next = m_tokens.peek();
	if (next.type == TokenType::Operator
		&& (next.data == "+" || next.data == "-")) {
		Expression exp(ExpressionType::Operator);
		exp.data = Expression::OperatorData { op_type_from_token(m_tokens.get()) };
		exp.children.push_back(term);
		exp.children.push_back(parse_exp_stage1());
		return exp;
	} else {
		return term;
	}
}

Expression Parser::parse_expression() {
	const auto term = parse_exp_stage1();
	const auto& next = m_tokens.peek();
	if (next.type == TokenType::Assign) {
		m_tokens.get();
		Expression exp(ExpressionType::Assignment);
		exp.children.push_back(term);
		exp.children.push_back(parse_expression());
		return exp;
	} else {
		return term;
	}
}

void Compiler::compile() {
	for (auto& function : m_parser.m_functions) {
		compile_function(function);
	}
}

void Compiler::compile_function(Function& function) {
	write("{}:", function.name);
	m_cur_function = &function;
	m_var_counter = 0;
	m_variables.clear();
	if (function.scope.variables.size()) {
		write("push ebp");
		write("mov ebp, esp");
		write("sub esp, {}", function.scope.variables.size() * 4);
	}
	for (auto& statement : function.statements) {
		compile_statement(statement);
	}
	m_cur_function = nullptr;
	write("");
}

void Compiler::compile_statement(Statement& statement) {
	if (statement.type == StatementType::Return) {
		// output should be in eax
		compile_expression(statement.expressions[0]);
		if (m_cur_function && m_cur_function->scope.variables.size()) {
			write("add esp, {}", m_cur_function->scope.variables.size() * 4);
			write("mov esp, ebp");
			write("pop ebp");
		}
		write("ret");
	} else if (statement.type == StatementType::Expression) {
		compile_expression(statement.expressions[0]);
	} else {
		assert(false, "unimplemented");
	}
}

void Compiler::compile_expression(Expression& exp) {
	if (exp.type == ExpressionType::Literal) {
		// the
		const auto value = std::get<int>(std::get<Expression::LiteralData>(exp.data).value);
		write("mov eax, {}", value);
	} else if (exp.type == ExpressionType::Operator) {
		const auto& data = std::get<Expression::OperatorData>(exp.data);
		if (data.op_type == OperatorType::Negation) {
			compile_expression(exp.children[0]);
			write("neg eax");
		} else if (data.op_type == OperatorType::Bitflip) {
			compile_expression(exp.children[0]);
			write("not eax");
		} else if (data.op_type == OperatorType::Not) {
			compile_expression(exp.children[0]);
			write("cmp eax, 0");
			write("mov eax, 0");
			write("sete al");
		} else if (data.op_type == OperatorType::Addition) {
			compile_expression(exp.children[0]);
			write("push eax");
			compile_expression(exp.children[1]);
			write("pop ecx");
			write("add eax, ecx");
		} else if (data.op_type == OperatorType::Subtraction) {
			compile_expression(exp.children[0]);
			write("push eax");
			compile_expression(exp.children[1]);
			write("pop ecx");
			write("sub ecx, eax");
			write("mov eax, ecx");
		} else if (data.op_type == OperatorType::Multiplication) {
			compile_expression(exp.children[0]);
			write("push eax");
			compile_expression(exp.children[1]);
			write("pop ecx");
			write("imul eax, ecx");
		} else {
			assert(false, "unimplemented");
		}
	} else if (exp.type == ExpressionType::Declaration) {
		const auto& data = std::get<Expression::DeclarationData>(exp.data);
		// stores a pointer to the variable in eax because idk how to deal with this yet
		write("lea eax, [ebp - {}]", m_var_counter * 4); // hardcode every variable to be 4 bytes trololol
		m_variables[data.var.name] = m_var_counter;
		++m_var_counter;
	} else if (exp.type == ExpressionType::Assignment) {
		compile_expression(exp.children[1]);
		write("push eax");
		// assumes its a var declaration, which stores a pointer to eax
		compile_expression(exp.children[0]);
		write("pop ecx");
		write("mov [eax], ecx");
		// assignment evaluates to the rhs
		write("mov eax, ecx");
	} else if (exp.type == ExpressionType::Variable) {
		const auto& data = std::get<Expression::VariableData>(exp.data);
		// no references yet
		write("mov eax, [ebp - {}]", m_variables[data.name] * 4);
	} else {
		print("{}\n", enum_name(exp.type));
		assert(false, "unimplemented");
	}
}


void print_expression(const Expression& exp, const int depth) {
	for (int i = 0; i < depth; ++i)
		print("  ");

	print("{} ", enum_name(exp.type));
	if (exp.type == ExpressionType::Literal) {
		const auto& value = std::get<Expression::LiteralData>(exp.data).value;
		std::visit([](const auto& value) {
			print("({}) ", value);
		}, value);
	} else if (exp.type == ExpressionType::Declaration) {
		const auto& var = std::get<Expression::DeclarationData>(exp.data).var;
		print("({}: {}) ", var.name, var.type.name);
	} else if (exp.type == ExpressionType::Variable) {
		print("({}) ", std::get<Expression::VariableData>(exp.data).name);
	} else if (exp.type == ExpressionType::Operator) {
		print("({}) ", enum_name(std::get<Expression::OperatorData>(exp.data).op_type));
	}
	print("\n");
	for (auto& child : exp.children) {
		print_expression(child, depth + 1);
	}
}

int main(int argc, char** argv) {
	auto args = ArrayView<char*>(argv, argc);
	if (args.size() < 2) {
		print(
			"Silly compiler for a silly language.\n\n"

			"Usage: {} input [output]\n\n"
			
			"  input - input file to compile\n",
			args[0]
		);
		return 1;
	}

	auto input_file = std::ifstream(args[1]);

	Lexer lexer(input_file);
	auto tokens = lexer.get_tokens();
	print("File tokenized\n");
	for (auto& token : tokens) {
		print(" - {}\n", token);
	}

	input_file.close();

	ArrayStream token_stream(ArrayView{tokens});
	Parser parser(args[1], token_stream);
	parser.parse();
	print("File parsed\n");

	for (auto& function : parser.m_functions) {
		print("Function {}: {}\n", function.name, function.return_type.name);
		for (auto& statement : function.statements) {
			print("  {}\n", enum_name(statement.type));
			for (auto& expression : statement.expressions) {
				print_expression(expression, 2);
			}
		}
	}

	std::stringstream stream;
	Compiler compiler(stream, parser);
	compiler.compile();

	print("Compiler finished\n");
	print("{}\n", stream.str());

	return 0;
}