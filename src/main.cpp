// could just include compiler.hpp since that includes everything else
#include "lexer.hpp"
#include "parser.hpp"
#include "checker.hpp"
#include "compiler.hpp"
#include "evaluator.hpp"

#include "enums.hpp"
#include "format.hpp"

void print_expression(const Expression& exp, const int depth = 0) {
	for (int i = 0; i < depth; ++i)
		print("  ");

	print("{} ", exp.type);
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
		print("({}) ", std::get<Expression::OperatorData>(exp.data).op_type);
	} else if (exp.type == ExpressionType::Call) {
		print("({}) ", std::get<Expression::CallData>(exp.data).function_name);
	}
	print("\n");
	for (auto& child : exp.children) {
		print_expression(child, depth + 1);
	}
}

auto& operator<<(std::ostream& stream, const Token& token) {
	stream << token.span.line << ':' << token.span.column << ' ';
	stream << enum_name(token.type);
	if (!token.data.empty()) stream << " \"" << token.data << '"';
	return stream;
}

int main(int argc, char** argv) {
	std::cout << std::boolalpha;

	auto args = ArrayView<char*>(argv, argc);
	if (args.size() < 2) {
		print(
			"tack compiler. very silly language\n"
			"\n"
			"Usage: {} input [opts]\n"
			"\n"
			"  input - input file to compile\n"
			"  opts:\n"
			"    -o output - output asm file\n"
			"    --show-tokens - prints lexer tokens\n"
			"    --show-ast - prints parser ast\n"
			"    --show-asm - prints output asm\n"
			"    --eval - uses evaluator\n"
			, args[0]
		);
		return 1;
	}

	bool show_tokens = false;
	bool show_ast = false;
	bool show_asm = false;
	bool evaluate = false;
	std::string output_file;
	auto rest = args.slice(2);
	for (size_t i = 0; i < rest.size(); ++i) {
		const std::string_view arg = rest[i];
		if (arg == "-o") {
			assert(i + 1 < rest.size(), "Expected file name");
			output_file = rest[i + 1];
			++i;
		} else if (arg == "--show-tokens") {
			show_tokens = true;
		} else if (arg == "--show-ast") {
			show_ast = true;
		} else if (arg == "--show-asm") {
			show_asm = true;
		} else if (arg == "--eval") {
			evaluate = true;
		} else {
			print("Unknown option \"{}\"\n", arg);
			return 1;
		}
	}

	auto input_file = std::ifstream(args[1]);
	if (!input_file.is_open()) {
		print("File \"{}\" could not be opened\n", args[1]);
		return 1;
	}

	Lexer lexer(input_file);
	auto tokens = lexer.get_tokens();
	print("File tokenized\n");
	if (show_tokens) {
		for (auto& token : tokens) {
			print(" - {}\n", token);
		}
	}

	input_file.close();

	ArrayStream token_stream(ArrayView{tokens});
	Parser parser(args[1], token_stream);
	parser.parse();
	print("File parsed\n");

	if (show_ast) {
		for (auto& function : parser.m_functions) {
			print("Function {}: {}\n", function.name, function.return_type.name);
			for (auto& statement : function.statements) {
				print("  {}\n", enum_name(statement.type));
				for (auto& expression : statement.expressions) {
					print_expression(expression, 2);
				}
			}
		}
	}

	TypeChecker checker(parser);
	checker.check();

	if (evaluate) {
		Evaluator evaluator(parser);
		const int result = evaluator.run();
		print("Program returned: {}\n", result);
	} else {
		std::stringstream stream;
		Compiler compiler(stream, parser);
		compiler.compile();

		print("Compiler finished\n");

		if (!output_file.empty()) {
			std::ofstream file(output_file);
			file << 
				"section .text\n"
				"global _start\n"
				"\n"
				"_start:\n"
				"	call main\n"
				"	mov ebx, eax\n"
				"	mov al, 1\n"
				"	int 0x80\n"
				"\n"
				"; -- generated asm --\n\n";
			file << stream.str();
		} else if (show_asm) {
			print("{}\n", stream.str());
		}
	}

	return 0;
}
