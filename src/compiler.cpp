#include "compiler.hpp"
#include "enums.hpp"
#include "format.hpp"

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
	for (size_t i = 0; i < function.arguments.size(); ++i) {
		m_variables[function.arguments[i].name] = -(function.arguments.size() - i - 1 + 2);
	}
	if (function.scope.variables.size() || function.arguments.size()) {
		write("push ebp");
		write("mov ebp, esp");
		if (function.scope.variables.size())
			write("sub esp, {}", function.scope.variables.size() * 4);
	}
	for (auto& statement : function.statements) {
		compile_statement(statement);
	}
	generate_return(function);
	m_cur_function = nullptr;
	write("");
}

void Compiler::compile_statement(Statement& statement) {
	if (statement.type == StatementType::Return) {
		if (!statement.expressions.empty()) {
			// output should be in eax
			compile_expression(statement.expressions[0]);
		}
		// shouldnt ever be null
		if (m_cur_function)
			generate_return(*m_cur_function);
	} else if (statement.type == StatementType::Expression) {
		compile_expression(statement.expressions[0]);
	} else {
		assert(false, "unimplemented");
	}
}

void Compiler::generate_return(const Function& function) {
	if (function.scope.variables.size()) {
		write("add esp, {}", function.scope.variables.size() * 4);
	}
		
	if (function.scope.variables.size() || function.arguments.size()) {
		write("mov esp, ebp");
		write("pop ebp");
	}
	write("ret");
}

void Compiler::compile_expression(Expression& exp, bool by_reference) {
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
		compile_expression(exp.children[0], true);
		write("pop ecx");
		write("mov [eax], ecx");
		// assignment evaluates to the rhs
		write("mov eax, ecx");
	} else if (exp.type == ExpressionType::Variable) {
		const auto& data = std::get<Expression::VariableData>(exp.data);
		if (!m_variables.count(data.name))
			assert(false, "unknown variable!");
		static constexpr auto format_offset = [](int off) -> std::string {
			if (off == 0) return "";
			else if (off < 0) return format("- {}", off);
			else return format("+ {}", off);
		};
		if (by_reference)
			write("lea eax, [ebp {}]", format_offset(-m_variables[data.name] * 4));
		else
			write("mov eax, [ebp {}]", format_offset(-m_variables[data.name] * 4));
	} else if (exp.type == ExpressionType::Call) {
		for (auto& child : exp.children) {
			compile_expression(child);
			write("push eax");
		}
		const auto& target_name = std::get<Expression::CallData>(exp.data).function_name;
		write("call {}", target_name);
		// clean up stack if theres arguments
		for (const auto& function : m_parser.m_functions) {
			if (function.name == target_name && !function.arguments.empty()) {
				write("add esp, {}", function.arguments.size() * 4);
				break;
			}
		}
	} else {
		print("{}\n", exp.type);
		assert(false, "unimplemented");
	}
}
