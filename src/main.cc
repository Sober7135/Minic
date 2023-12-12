#include "lexer.hh"
#include "tokenType.hh"

#include <FlexLexer.h>
#include <format>
#include <fstream>
#include <iostream>

auto main(int argc, char *argv[]) -> int {
  if (argc != 2) {
    std::cerr << "Error!\n";
    return 1;
  }
  auto is = std::ifstream(argv[1]);
  auto lex = new yyFlexLexer(is, std::cout);
  int token{};
  do {
    token = lex->yylex();
    std::cout << std::format("{}\n", Token2String(yylval));
  } while (token != TokenType::EndOfFile);
}