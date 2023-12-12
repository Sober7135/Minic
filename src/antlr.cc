#include "ANTLRInputStream.h"
#include "CommonTokenStream.h"
#include "MinicLexer.h"
#include "MinicParser.h"

#include <fstream>
#include <iostream>

using namespace antlr4;

auto main(int argc, char *argv[]) -> int {
  std::ifstream Stream(argv[1]);
  ANTLRInputStream Input(Stream);
  MinicLexer Lexer(&Input);
  CommonTokenStream Tokens(&Lexer);
  Tokens.fill();

  // for (auto &token : Tokens.getTokens()) {
  //   std::cout << token->toString() << "\n";
  //   std::cout << Lexer.getVocabulary().getSymbolicName(token->getType())
  //             << "\n";
  // }

  MinicParser Parser(&Tokens);
  auto *Program = Parser.program();
  std::cout << Program->toStringTree(&Parser, true);
}