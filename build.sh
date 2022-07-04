#!/bin/sh

clang++ src/lexer.cpp src/parser.cpp src/checker.cpp src/compiler.cpp src/main.cpp src/utils.cpp -std=c++20 \
	 -g -pedantic -Wall -Wno-missing-braces -fsanitize=address -fsanitize=undefined -o main