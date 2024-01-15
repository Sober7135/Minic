#include <llvm/Support/raw_ostream.h>

#include <cstdio>
#include <format>
#include <fstream>
#include <memory>
#include <system_error>

#include "ANTLRInputStream.h"
#include "ASTBuilder.hh"
#include "CommonTokenStream.h"
#include "MinicLexer.h"
#include "MinicParser.h"
#include "Target.hh"
#include "Visitor.hh"

using namespace antlr4;

auto main([[maybe_unused]] int argc, char* argv[]) -> int {
  // TODO  Parse Command Line Arguments
  std::error_code EC;
  std::ifstream Stream(argv[1]);
  ANTLRInputStream Input(Stream);
  MinicLexer Lexer(&Input);
  CommonTokenStream Tokens(&Lexer);
  Tokens.fill();

  for (auto& token : Tokens.getTokens()) {
    // llvm::outs() << std::format(
    //     "{} <{}>\n",
    //     token->toString(),
    //     Lexer.getVocabulary().getSymbolicName(token->getType())
    // );

    llvm::outs() << std::format(
        "[@{},'{}',<{}>,Ln{},Col{}]\n",
        token->getTokenIndex(),
        token->getText(),
        Lexer.getVocabulary().getSymbolicName(token->getType()),
        token->getLine(),
        token->getCharPositionInLine()
    );
  }

  MinicParser Parser(&Tokens);
  auto* Program = Parser.program();
  // llvm::outs() << Program->toStringTree(&Parser, true);
  Minic::ASTBuilder TheASTBuilder(Program);
  TheASTBuilder.Build();

  // AST
  auto& AST = TheASTBuilder.AST();
  auto Printer = Minic::ASTPrinter(std::string(argv[1]) + ".ast", EC);
  Printer.Visit(AST);
  Minic::CodeGenVisitor CGV(argv[1]);
  CGV.Visit(AST);

  auto outToFile =
      std::make_unique<llvm::raw_fd_ostream>(std::string(argv[1]) + ".ll", EC);
  CGV.LW->Mod->print(*outToFile, nullptr);

  Minic::genObjectFile(CGV);
}