#pragma once
#include "parser.hpp"
#include <unordered_map>

class Compiler {
public:
	std::ostream& m_stream;
	Parser& m_parser;
	Function* m_cur_function = nullptr;
	// TODO: not
	size_t m_var_counter = 0;
	std::unordered_map<std::string, int> m_variables;
	size_t m_label_counter = 0;

	Compiler(std::ostream& output, Parser& parser) : m_stream(output), m_parser(parser) {}

	template <class... Args>
	void write(const std::string_view& format, Args&&... args) {
		format_to(m_stream, format, args...);
		m_stream << '\n';
	}

	void compile_expression(Expression&, bool ref = false);
	void compile_statement(Statement&);
	void compile_function(Function&);

	void generate_return(const Function& function);

	void compile();
};
