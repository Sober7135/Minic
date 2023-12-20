#include "Visitor.hh"

#include "AST.hh"
#include "Utils.hh"
#include <cassert>
#include <cstddef>
#include <string>

namespace Minic {

/* =============================== ASTVisitor =============================== */
// auto ASTVisitor::Visit(ASTNode *Node) -> void { Node->accept(this); }
// auto ASTVisitor::Visit(Declaration *Node) -> void { Node->accept(this); }

/* =============================== ASTPrinter =============================== */
auto ASTPrinter::Visit(const Program &TheProgram) -> void {
  Out << "AST:\n";
  for (const auto &TheDeclaration : TheProgram) {
    Visit(TheDeclaration.get());
  }
}

auto ASTPrinter::Visit(ASTNode *Node) -> void { Node->accept(this); }

auto ASTPrinter::Visit(Declaration *Node) -> void { Node->accept(this); }

auto ASTPrinter::Visit(VarDecl *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += 2;
  assert(Node->TheDeclaratorList.size() == Node->TheInitializerList.size());
  for (size_t i = 0; i < Node->TheDeclaratorList.size(); ++i) {
    Visit(Node->TheDeclaratorList[i].get());
    Out << "\n";
    I += 2;
    Visit(Node->TheInitializerList[i].get());
    Out << "\n";
    I -= 2;
  }
  I -= 2;
}

auto ASTPrinter::Visit(FunctionDecl *Node) -> void {
  // Top Level
  Out << StringWrapper(std::string(*Node), I) << '\n';

  if (!Node->isPrototype()) {
    I += 2;
    // CompoundStmt
    Visit(Node->Body.get());
    I -= 2;
  }
}

auto ASTPrinter::Visit(ParmVarDecl *Node) -> void {
  Out << StringWrapper(std::string(*Node)) << '\n';
}

auto ASTPrinter::Visit(Expr *Node) -> void { Node->accept(this); }

auto ASTPrinter::Visit(VariableExpr *Node) -> void {
  Out << StringWrapper(std::string(*Node), I);
}

auto ASTPrinter::Visit(CallExpr *Node) -> void {
  Out << StringWrapper(std::string(*Node), I);
}

auto ASTPrinter::Visit(UnaryExpr *Node) -> void {
  Out << StringWrapper(std::string(*Node));
}

auto ASTPrinter::Visit(BinaryExpr *Node) -> void {
  Out << StringWrapper(std::string(*Node));
}

auto ASTPrinter::Visit(LiteralExpr *Node) -> void { Node->accept(this); }

auto ASTPrinter::Visit(LiteralIntegerExpr *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(LiteralFloatExpr *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(LiteralCharExpr *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(Statement *Node) -> void { Node->accept(this); }

auto ASTPrinter::Visit(ExprStmt *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(IfStmt *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += 2;
  Visit(Node->IfBody.get());
  if (Node->ElseBody) {
    Visit(Node->ElseBody.get());
  }
  I -= 2;
}

auto ASTPrinter::Visit(WhileStmt *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += 2;
  Visit(Node->Body.get());
  I -= 2;
}

auto ASTPrinter::Visit(ReturnStmt *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(BreakStmt *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(ContinueStmt *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(VarDeclStmt *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += 2;
  Visit(Node->TheVarDecl.get());
  I -= 2;
}

auto ASTPrinter::Visit(CompoundStmt *Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += 2;
  for (const auto &StmtPtr : Node->Statements) {
    // Statements
    Visit(StmtPtr.get());
  }
  I -= 2;
}

auto ASTPrinter::Visit(Declarator *Node) -> void {
  Out << StringWrapper(std::string(*Node), I);
}

auto ASTPrinter::Visit(Initializer *Node) -> void {
  if (!Node) {
    return;
  }
  Out << StringWrapper(std::string(*Node), I);
}

} // namespace Minic