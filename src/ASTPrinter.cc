#include <llvm/Support/raw_ostream.h>

#include <cassert>
#include <cstddef>
#include <format>
#include <string>

#include "AST.hh"
#include "Utils.hh"
#include "Visitor.hh"

namespace Minic {

/* =============================== ASTVisitor =============================== */
// auto ASTVisitor::Visit(ASTNode *Node) -> void { Node->accept(this); }
// auto ASTVisitor::Visit(Declaration *Node) -> void { Node->accept(this); }

/* =============================== ASTPrinter =============================== */
auto ASTPrinter::Visit(const Program& TheProgram) -> void {
  Out << "Program\n";
  I += UINDENT;
  for (const auto& TheDeclaration : TheProgram) {
    Visit(TheDeclaration.get());
  }
  I -= UINDENT;
}

auto ASTPrinter::Visit(ASTNode* Node) -> void {
  Node->accept(this);
}

auto ASTPrinter::Visit(Declaration* Node) -> void {
  Node->accept(this);
}

auto ASTPrinter::Visit(VarDecl* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += UINDENT;
  assert(Node->TheDeclaratorList.size() == Node->TheInitializerList.size());
  for (size_t i = 0; i < Node->TheDeclaratorList.size(); ++i) {
    Visit(Node->TheDeclaratorList[i].get());
    Out << "\n";
    I += UINDENT;
    Visit(Node->TheInitializerList[i].get());
    I -= UINDENT;
  }
  I -= UINDENT;
}

auto ASTPrinter::Visit(FunctionDecl* Node) -> void {
  // Top Level
  Out << StringWrapper(std::string(*Node), I) << '\n';
  I += UINDENT;
  Out << StringWrapper(std::string("ParmVarDecl"), I) << '\n';

  I += UINDENT;
  if (Node->VarList.size()) {
    for (size_t i = 0, end = Node->VarList.size(); i != end; ++i) {
      Out << StringWrapper(
          std::format(
              "{}: {}",
              std::to_string(i),
              std::string(*Node->VarList[i])
          ),
          I
      ) << '\n';
    }
  } else {
    Out << StringWrapper(std::format("0: void"), I) << '\n';
  }

  I -= 2 * UINDENT;

  if (!Node->isPrototype()) {
    I += UINDENT;
    // CompoundStmt
    Visit(Node->Body.get());
    I -= UINDENT;
  }
}

auto ASTPrinter::Visit(ParmVarDecl* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(Expr* Node) -> void {
  Node->accept(this);
}

auto ASTPrinter::Visit(VariableExpr* Node) -> void {
  Out << StringWrapper(std::string(*Node), I);
}

auto ASTPrinter::Visit(CallExpr* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(ArraySubscriptExpr* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(UnaryExpr* Node) -> void {
  Out << StringWrapper(std::string(*Node));
}

auto ASTPrinter::Visit(BinaryExpr* Node) -> void {
  Out << StringWrapper(std::string(*Node), I);
}

auto ASTPrinter::Visit(LiteralExpr* Node) -> void {
  Node->accept(this);
}

auto ASTPrinter::Visit(LiteralIntegerExpr* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(LiteralFloatExpr* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(LiteralCharExpr* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(Statement* Node) -> void {
  Node->accept(this);
}

auto ASTPrinter::Visit(ExprStmt* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += UINDENT;
  Visit(Node->TheExpr.get());
  I -= UINDENT;
}

auto ASTPrinter::Visit(IfStmt* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += UINDENT;
  Visit(Node->IfBody.get());
  if (Node->ElseBody) {
    Visit(Node->ElseBody.get());
  }
  I -= UINDENT;
}

auto ASTPrinter::Visit(WhileStmt* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += UINDENT;
  Visit(Node->Body.get());
  I -= UINDENT;
}

auto ASTPrinter::Visit(ReturnStmt* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(BreakStmt* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(ContinueStmt* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';
}

auto ASTPrinter::Visit(VarDeclStmt* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += UINDENT;
  Visit(Node->TheVarDecl.get());
  I -= UINDENT;
}

auto ASTPrinter::Visit(CompoundStmt* Node) -> void {
  Out << StringWrapper(std::string(*Node), I) << '\n';

  I += UINDENT;
  for (const auto& StmtPtr : Node->Statements) {
    // Statements
    Visit(StmtPtr.get());
  }
  I -= UINDENT;
}

auto ASTPrinter::Visit(Declarator* Node) -> void {
  Out << StringWrapper(std::string(*Node), I);
}

auto ASTPrinter::Visit(Initializer* Node) -> void {
  if (!Node) {
    return;
  }
  // Out << StringWrapper(std::string(*Node), I) << '\n';
  if (Node->isLeaf()) {
    Visit(Node->TheExpr.get());
    return;
  }

  Out << StringWrapper(std::string("Initializer"), I) << '\n';

  I += UINDENT;
  for (const auto& c : Node->Children) {
    Visit(c.get());
  }
  I -= UINDENT;
}

}  // namespace Minic