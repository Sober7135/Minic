#include "ANTLRInputStream.h"
#include "ASTBuilder.hh"
#include "CommonTokenStream.h"
#include "MinicLexer.h"
#include "MinicParser.h"
#include "Visitor.hh"

#include <fstream>
#include <iostream>
#include <llvm/Support/raw_ostream.h>

using namespace antlr4;

auto main(int argc, char *argv[]) -> int {
  // TODO  Parse Command Line Arguments
  std::ifstream Stream(argv[1]);
  ANTLRInputStream Input(Stream);
  MinicLexer Lexer(&Input);
  CommonTokenStream Tokens(&Lexer);
  Tokens.fill();

  // for (auto &token : Tokens.getTokens()) {
  //   std::cout << token->toString() << "\n";
  //   std::cout << Lexer.getVocabulary().getSymbolicName(token->getType())
  //             << "\n";
  //   ;
  // }

  MinicParser Parser(&Tokens);
  auto *Program = Parser.program();
  // std::cout << Program->toStringTree(&Parser, true) << std::endl;
  Minic::ASTBuilder TheASTBuilder(Program);
  TheASTBuilder.Build();
  auto &AST = TheASTBuilder.AST();
  auto Printer = Minic::ASTPrinter(llvm::outs());
  // Printer.Visit(AST);
  Minic::CodeGenVisitor CGV;
  CGV.Visit(AST);
  CGV.LW->Mod->print(llvm::outs(), nullptr);
}