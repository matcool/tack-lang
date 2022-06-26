clang src/lexer.cpp src/parser.cpp src/compiler.cpp src/main.cpp -std=c++20 -pedantic -Wall -Wno-missing-braces -fsanitize=address -fsanitize=undefined -o main.exe &&^
main.exe foo.lang