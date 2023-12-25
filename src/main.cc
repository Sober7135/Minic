#include "ANTLRInputStream.h"
#include "ASTBuilder.hh"
#include "CommonTokenStream.h"
#include "MinicLexer.h"
#include "MinicParser.h"
#include "Target.hh"
#include "Visitor.hh"

#include <cstdio>
#include <fstream>
#include <llvm/Support/raw_ostream.h>

using namespace antlr4;

// TODO frontend Support LogicalAnd ... Xor Mod....
auto main([[maybe_unused]]int argc, char *argv[]) -> int {
  // TODO  Parse Command Line Arguments
  std::ifstream Stream(argv[1]);
  ANTLRInputStream Input(Stream);
  MinicLexer Lexer(&Input);
  CommonTokenStream Tokens(&Lexer);
  Tokens.fill();

  // for (auto &token : Tokens.getTokens()) {
  //   llvm::outs() << token->toString() << "\n";
  //   llvm::outs() << Lexer.getVocabulary().getSymbolicName(token->getType())
  //                << "\n";
  //   ;
  // }

  MinicParser Parser(&Tokens);
  auto *Program = Parser.program();
  // llvm::outs() << Program->toStringTree(&Parser, true);
  fflush(stdout);
  Minic::ASTBuilder TheASTBuilder(Program);
  TheASTBuilder.Build();
  auto &AST = TheASTBuilder.AST();
  auto Printer = Minic::ASTPrinter(llvm::outs());
  Printer.Visit(AST);
  Minic::CodeGenVisitor CGV(argv[1]);
  CGV.Visit(AST);
  llvm::outs() << "IR:\n";
  CGV.LW->Mod->print(llvm::outs(), nullptr);
  Minic::genObjectFile(CGV);
}