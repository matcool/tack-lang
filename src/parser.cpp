#include "parser.hpp"
#include "format.hpp"


Parser::Parser(const std::string_view& file_name, ArrayStream<Token> tokens)
	: m_file_name(file_name),
	m_tokens(tokens) {}

void Parser::error_at_token(const Token& token, const std::string_view& msg) {
	print("[error] {}", msg);
	if (!m_file_name.empty())
		print_file_span(m_file_name, token.span);
	print('\n');
	std::exit(1);
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

			parse_comma_list([&] {
				function.arguments.push_back(parse_var_decl());
			});

			if (m_tokens.peek().type == TokenType::TypeIndicator) {
				m_tokens.get();
				function.return_type = parse_type();
				expect_token_type(m_tokens.get(), TokenType::LeftBracket, "Expected bracket");
			} else if (m_tokens.peek().type == TokenType::LeftBracket) {
				m_tokens.get();
				function.return_type = Type { "void" };
			} else {
				expect_token_type(m_tokens.get(), TokenType::LeftBracket, "Expected bracket or type indicator");
			}
			
			parse_function(function);
		} else {
			assert(false, "Unimplemented token outside in global scope");
		}
	}
}

Type Parser::parse_type() {
	// TODO: fancier types
	const auto token = expect_token_type(m_tokens.get(), TokenType::Identifier, "Expected type");
	return Type { token.data };
}

Variable Parser::parse_var_decl() {
	auto name_token = m_tokens.get();
	if (name_token.type != TokenType::Identifier)
		error_at_token(name_token, "Expected variable name");
	if (m_tokens.get().type != TokenType::TypeIndicator)
		error_at_token(m_tokens.prev(), "Expected type indicator");
	const auto type = parse_type();

	return Variable { type, name_token.data };
}

void Parser::parse_function(Function& function) {
	m_cur_function = &function;
	while (m_tokens.peek().type != TokenType::RightBracket) {
		const auto stmt = parse_statement();
		function.statements.push_back(stmt);
		// TODO: uhh not this
		if (stmt.type != StatementType::If)
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
		Statement stmt { StatementType::Return };
		stmt.span = first.span;
		if (m_tokens.peek().type != TokenType::Semicolon)
			stmt.expressions.push_back(parse_expression());
		return stmt;
	} else if (first.type == TokenType::Keyword && first.data == "if") {
		m_tokens.get();
		Statement stmt { StatementType::If };
		stmt.span = first.span;
		stmt.expressions.push_back(parse_expression());
		stmt.data = Statement::IfData { .children = parse_block() };
		return stmt;
	} else {
		const auto exp = parse_expression();
		return Statement {
			StatementType::Expression,
			{ exp }
		};
	}
}

std::vector<Statement> Parser::parse_block() {
	std::vector<Statement> result;
	expect_token_type(m_tokens.get(), TokenType::LeftBracket, "Expected left bracket");
	while (m_tokens.peek().type != TokenType::RightBracket) {
		result.push_back(parse_statement());
		expect_token_type(m_tokens.get(), TokenType::Semicolon, "Expected semicolon");
	}
	m_tokens.get(); // should be right bracket
	return result;
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
		if (m_tokens.peek().type == TokenType::LeftParen) {
			m_tokens.get();
			Expression exp(ExpressionType::Call);
			exp.data = Expression::CallData { token.data };
			parse_comma_list([&] {
				exp.children.push_back(parse_expression());
			});
			return exp;
		} else {
			Expression exp(ExpressionType::Variable);
			exp.data = Expression::VariableData { token.data };
			return exp;
		}
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
		return Expression {
			ExpressionType::Declaration,
			Expression::DeclarationData { var },
			{}
		};
	} else if (token.type == TokenType::Keyword && (token.data == "true" || token.data == "false")) {
		return Expression {
			ExpressionType::Literal,
			Expression::LiteralData { token.data == "true" },
			{}
		};
	} else {
		error_at_token(token, "Tried to parse unknown primary expression");
	}
	std::abort();
}

Expression Parser::parse_exp_stage2() {
	const auto span = m_tokens.peek().span;
	auto factor = parse_exp_primary();
	factor.span = span;
	const auto& next = m_tokens.peek();
	if (next.type == TokenType::Operator
		&& (next.data == "/" || next.data == "*")) {
		Expression exp(ExpressionType::Operator);
		exp.span = next.span;
		exp.data = Expression::OperatorData { op_type_from_token(m_tokens.get()) };
		exp.children.push_back(factor);
		exp.children.push_back(parse_exp_stage2());
		return exp;
	} else {
		return factor;
	}
}

Expression Parser::parse_exp_stage1() {
	const auto span = m_tokens.peek().span;
	auto term = parse_exp_stage2();
	term.span = span;
	const auto& next = m_tokens.peek();
	if (next.type == TokenType::Operator
		&& (next.data == "+" || next.data == "-")) {
		Expression exp(ExpressionType::Operator);
		exp.span = next.span;
		exp.data = Expression::OperatorData { op_type_from_token(m_tokens.get()) };
		exp.children.push_back(term);
		exp.children.push_back(parse_exp_stage1());
		return exp;
	} else {
		return term;
	}
}

Expression Parser::parse_expression() {
	const auto span = m_tokens.peek().span;
	auto term = parse_exp_stage1();
	term.span = span;
	const auto& next = m_tokens.peek();
	if (next.type == TokenType::Assign) {
		m_tokens.get();
		Expression exp(ExpressionType::Assignment);
		exp.span = next.span;
		exp.children.push_back(term);
		exp.children.push_back(parse_expression());
		return exp;
	} else {
		return term;
	}
}
