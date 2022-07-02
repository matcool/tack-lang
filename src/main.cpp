// could just include compiler.hpp since that includes everything else
#include "lexer.hpp"
#include "parser.hpp"
#include "checker.hpp"
#include "compiler.hpp"

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

int main(int argc, char** argv) {
	std::cout << std::boolalpha;

	auto args = ArrayView<char*>(argv, argc);
	if (args.size() < 2) {
		print(
			"Silly compiler for a silly language.\n\n"

			"Usage: {} input [output]\n\n"
			
			"  input - input file to compile\n"
			"  output - output asm file, if not provided will just print to stdout\n",
			args[0]
		);
		return 1;
	}

	auto input_file = std::ifstream(args[1]);

	Lexer lexer(input_file);
	auto tokens = lexer.get_tokens();
	print("File tokenized\n");
	// for (auto& token : tokens) {
	// 	print(" - {}\n", token);
	// }

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

	TypeChecker checker(parser);
	checker.check();

	std::stringstream stream;
	Compiler compiler(stream, parser);
	compiler.compile();

	print("Compiler finished\n");

	if (args.size() > 2) {
		std::ofstream file(args[2]);
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
	} else {
		print("{}\n", stream.str());
	}

	return 0;
}
