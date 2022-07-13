#pragma once
#include <algorithm>
#include <optional>
#include <stdint.h>
#include <vector>
#include "format.hpp"

// TODO: for some reason i dont have std::source_location in -std=c++20 yet
namespace {
	struct SourceLocation {
		std::string_view file_name;
		size_t line;

		static auto current(const size_t line = __builtin_LINE(), const char* const file = __builtin_FILE()) {
			return SourceLocation { file, line };
		}
	};
}

inline constexpr void assert(bool value, const std::string_view msg, const SourceLocation location = SourceLocation::current()) {
	if (!value) {
		print("{}:{}: Assertion failed `{}`\n", location.file_name, location.line, msg);
		std::abort();
	}
}

[[noreturn]] 
inline void unhandled(const std::string_view msg, const SourceLocation location = SourceLocation::current()) {
	print("{}:{}: FIXME: `{}`\n", location.file_name, location.line, msg);
	std::abort();
}


template <class T>
class ArrayView {
	T* m_data;
	size_t m_size;
public:

	auto size() const { return m_size; }
	auto data() { return m_data; }

	ArrayView() : m_data(nullptr), m_size(0) {}
	ArrayView(T* data, const size_t size) : m_data(data), m_size(size) {}
	ArrayView(std::vector<T>& vec) : ArrayView(vec.data(), vec.size()) {}
	T& operator[](const size_t i) { return m_data[i]; }
	T* begin() { return m_data; }
	T* end() { return m_data + m_size; }
	auto slice(const size_t start, const size_t end = -1) { return ArrayView(m_data + start, std::min(end, m_size) - start); }

	template <class P>
	std::optional<size_t> find(P&& predicate) {
		for (size_t i = 0; i < m_size; ++i)
			if (predicate(m_data[i])) return i;
		return {};
	}

	template <class P>
	std::optional<size_t> rfind(P&& predicate) {
		for (size_t i = m_size; i; --i)
			if (predicate(m_data[i - 1])) return i - 1;
		return {};
	}
};

template <class T>
class ArrayStream {
public:
	size_t m_pos = 0;
	ArrayView<T> m_view;
	ArrayStream(ArrayView<T> view) : m_view(view) {}

	auto size() const { return m_view.size() - m_pos; }
	auto view() {
		return m_view.slice(m_pos);
	}

	T& peek() {
		assert(size(), "Out of bounds");
		return m_view[m_pos];
	}
	T& prev() {
		assert(m_pos, "Out of bounds");
		return m_view[m_pos - 1];
	}
	T& get() {
		assert(size(), "Out of bounds");
		return m_view[m_pos++];
	}
	auto pos() const {
		return m_pos;
	}
	void seek_rel(int amt) {
		if (amt < 0)
			assert(-amt <= int(m_pos), "Out of bounds");
		m_pos += amt;
	}
	void seek_to(size_t pos) {
		assert(pos < m_view.size(), "Out of bounds");
		m_pos = pos;
	}
};

// Dos not work!!! do not use
template <class T>
class Enumerator {
public:
	template <class I>
	struct EnumeratorIterator {
		size_t index;
		I iterator;

		EnumeratorIterator(size_t index, I iterator) : index(index), iterator(iterator) {}
		
		auto& operator++() {
			++index;
			iterator++;
			return *this;
		}

		bool operator!=(const EnumeratorIterator& other) {
			return iterator != other.iterator;
		}

		auto operator*() {
			return std::make_pair(index, *iterator);
		}
	};
	T& m_container;
	Enumerator(T& container) : m_container(container) {}
	auto begin() { auto i = m_container.begin(); return EnumeratorIterator<decltype(i)>(0, i); }
	auto end() { auto i = m_container.end(); return EnumeratorIterator<decltype(i)>(-1, i); }
};

template <class T>
auto enumerate(T&& container) {
	return Enumerator(std::forward<T>(container));
}

struct Span;

void print_file_span(const std::string& file_name, const Span& span);

// from: https://en.cppreference.com/w/cpp/utility/variant/visit
template <class... Ts>
struct overloaded : Ts... {
	using Ts::operator()...;
};
// explicit deduction guide (not needed as of C++20)
template <class... Ts> overloaded(Ts...) -> overloaded<Ts...>;