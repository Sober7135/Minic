#include "lexer.hh"
#include <FlexLexer.h>
#include <format>
#include <fstream>
#include <iostream>

auto main() -> int {
  auto is = std::ifstream("../test/lexer/all.txt");
  auto lex = new yyFlexLexer(is, std::cout);
  int token;
  while ((token = lex->yylex()) != TokenType::kEndOfFile) {
    std::cout << std::format("{}\n", token);
  }
}