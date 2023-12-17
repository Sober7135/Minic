#include "AST.hh"
#include "Visitor.hh"

namespace Minic {

/* =============================== CodeGenVisitor ============================*/
auto CodeGenVisitor::Visit(const Program &TheProgram) -> void {
  for (const auto &Decl : TheProgram) {
    Visit(Decl.get());
  }
}

auto CodeGenVisitor::Visit(ASTNode *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(Declaration *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(VarDecl *Node) -> void {
  // TODO
}

auto CodeGenVisitor::Visit(FunctionDecl *Node) -> void {}

auto CodeGenVisitor::Visit(ParmVarDecl *Node) -> void {}

auto CodeGenVisitor::Visit(Expr *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(VariableExpr *Node) -> void {}

auto CodeGenVisitor::Visit(CallExpr *Node) -> void {}

auto CodeGenVisitor::Visit(UnaryExpr *Node) -> void {}

auto CodeGenVisitor::Visit(BinaryExpr *Node) -> void {}

auto CodeGenVisitor::Visit(LiteralExpr *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(LiteralIntegerExpr *Node) -> void {}

auto CodeGenVisitor::Visit(LiteralFloatExpr *Node) -> void {}

auto CodeGenVisitor::Visit(LiteralCharExpr *Node) -> void {}

auto CodeGenVisitor::Visit(Statement *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(ExprStmt *Node) -> void {}

auto CodeGenVisitor::Visit(IfStmt *Node) -> void {}

auto CodeGenVisitor::Visit(WhileStmt *Node) -> void {}

auto CodeGenVisitor::Visit(ReturnStmt *Node) -> void {}

auto CodeGenVisitor::Visit(BreakStmt *Node) -> void {}

auto CodeGenVisitor::Visit(ContinueStmt *Node) -> void {}

auto CodeGenVisitor::Visit(VarDeclStmt *Node) -> void {}

auto CodeGenVisitor::Visit(CompoundStmt *Node) -> void {}

auto CodeGenVisitor::Visit(Declarator *Node) -> void {}

auto CodeGenVisitor::Visit(Initializer *Node) -> void {
  if (!Node) {
    return;
  }
  // TODO
}
} // namespace Minic