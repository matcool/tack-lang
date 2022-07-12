cmake_minimum_required(VERSION 3.10.0)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)

project(tack LANGUAGES CXX)

add_executable(tack
	src/lexer.cpp
	src/parser.cpp
	src/checker.cpp
	src/compiler.cpp
	src/evaluator.cpp
	src/main.cpp
	src/utils.cpp
)

target_compile_features(tack PUBLIC cxx_std_20)

target_compile_options(tack PRIVATE -fsanitize=address,undefined)
target_link_options(tack PRIVATE -fsanitize=address,undefined)

target_compile_options(tack PRIVATE -Wall -Wpedantic -Wno-missing-braces)

set_target_properties(tack PROPERTIES CXX_CLANG_TIDY clang-tidy)
