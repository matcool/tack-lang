#pragma once
#include <string>
#include <sstream>
#include <string_view>
#include <functional>
#include <iostream>

template <class T, class... Args>
void format_to(T& output, const std::string_view& format, Args&&... args) {
	if constexpr (sizeof...(Args) == 0) {
		output << format;
	} else {
		std::function<void(T&)> partials[sizeof...(Args)] = { [&](T& s) { s << args; }... };
		size_t counter = 0;
		for (size_t i = 0; i < format.size(); ++i) {
			const auto c = format[i];
			if (c == '{') {
				const auto next = format[i + 1];
				if (next == '{') {
					output << c;
					++i;
				} else if (next == '}') {
					partials[counter++](output);
					++i;
				}
			} else if (c == '}' && format[i + 1] == '}') {
				++i;
				output << c;
			} else
				output << c;
		}
	}
}

template <class... Args>
std::string format(const std::string_view& format, Args&&... args) {
	std::stringstream stream;
	format_to(stream, format, args...);
	return stream.str();
}

template <class... Args>
void print(const std::string_view& format, Args&&... args) {
	format_to(std::cout, format, args...);
}

inline void print(const char c) {
	std::cout << c;
}