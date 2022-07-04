#include "utils.hpp"
#include "lexer.hpp"

void print_file_span(const std::string& file_name, const Span& span) {
	print(" @ {}:{}:{}\n", file_name, span.line, span.column);
	std::ifstream file(file_name);
	std::string line;
	size_t counter = 1;
	while (std::getline(file, line)) {
		if (counter == span.line)
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
	for (size_t i = 1; i < span.column; ++i)
		print(' ');
	print("^ here");
}