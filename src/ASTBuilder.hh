#pragma once

#include <any>
#include <memory>
#include <utility>

#include "AST.hh"
#include "MinicBaseVisitor.h"
#include "MinicParser.h"

namespace Minic {

class ASTBuilder: public MinicBaseVisitor {
  MinicParser::ProgramContext* TheProgramContext;
  Program TheProgram;

  auto visitBinaryExprHelper(
      antlr4::Token* Op,
      MinicParser::ExprContext* LHS,
      MinicParser::ExprContext* RHS
  ) -> void;

public:
  explicit ASTBuilder(MinicParser::ProgramContext* ctx) :
      TheProgramContext(ctx) {}

  [[nodiscard]] auto AST() const -> const Program& {
    return TheProgram;
  }

  auto Build() -> void;

  // auto visitProgram(MinicParser::ProgramContext *ctx) -> std::any override;
  // auto visitDeclaration(MinicParser::DeclarationContext *ctx)
  //     -> std::any override;
  auto visitGlobalVarDeclStmt(MinicParser::GlobalVarDeclStmtContext* ctx)
      -> std::any override;
  auto visitVarDeclStmt(MinicParser::VarDeclStmtContext* ctx)
      -> std::any override;
  auto visitVarDecl(MinicParser::VarDeclContext* ctx) -> std::any override;
  auto visitDataType(MinicParser::DataTypeContext* ctx) -> std::any override;
  auto visitInitDeclaratorList(MinicParser::InitDeclaratorListContext* ctx)
      -> std::any override;
  auto visitInitDeclarator(MinicParser::InitDeclaratorContext* ctx)
      -> std::any override;
  auto visitDeclarator(MinicParser::DeclaratorContext* ctx)
      -> std::any override;
  auto visitInitializer(MinicParser::InitializerContext* ctx)
      -> std::any override;
  auto visitLiteral(MinicParser::LiteralContext* ctx) -> std::any override;
  auto visitFunctionDecl(MinicParser::FunctionDeclContext* ctx)
      -> std::any override;
  auto visitParmVarList(MinicParser::ParmVarListContext* ctx)
      -> std::any override;
  auto visitParmVarDecl(MinicParser::ParmVarDeclContext* ctx)
      -> std::any override;
  auto visitCompoundStmt(MinicParser::CompoundStmtContext* ctx)
      -> std::any override;
  auto visitIfStmt(MinicParser::IfStmtContext* ctx) -> std::any override;
  auto visitWhileStmt(MinicParser::WhileStmtContext* ctx) -> std::any override;
  // not support, maybe later
  // auto visitForStmt(MinicParser::ForStmtContext *ctx) -> std::any
  // override;
  auto visitExprStmt(MinicParser::ExprStmtContext* ctx) -> std::any override;
  auto visitReturnStmt(MinicParser::ReturnStmtContext* ctx)
      -> std::any override;
  auto visitBreakStmt(MinicParser::BreakStmtContext* ctx) -> std::any override;
  auto visitContinueStmt(MinicParser::ContinueStmtContext* ctx)
      -> std::any override;
  auto visitBitwiseAndExpr(MinicParser::BitwiseAndExprContext* ctx)
      -> std::any override;
  auto visitBitwiseOrExpr(MinicParser::BitwiseOrExprContext* ctx)
      -> std::any override;
  auto visitMulModDivExpr(MinicParser::MulModDivExprContext* ctx)
      -> std::any override;
  auto visitRelationalENEExpr(MinicParser::RelationalENEExprContext* ctx)
      -> std::any override;
  auto visitUnaryExpr(MinicParser::UnaryExprContext* ctx) -> std::any override;
  auto visitLogicalAndExpr(MinicParser::LogicalAndExprContext* ctx)
      -> std::any override;
  auto visitRelationalLGExpr(MinicParser::RelationalLGExprContext* ctx)
      -> std::any override;
  auto visitAssignExpr(MinicParser::AssignExprContext* ctx)
      -> std::any override;
  auto visitLogicalOrExpr(MinicParser::LogicalOrExprContext* ctx)
      -> std::any override;
  auto visitIdentifierExpr(MinicParser::IdentifierExprContext* ctx)
      -> std::any override;
  auto visitArraySubscriptExpr(MinicParser::ArraySubscriptExprContext* ctx)
      -> std::any override;
  // auto visitLiteralExpr(MinicParser::LiteralExprContext *ctx)
  //     -> std::any override;
  auto visitCallExpr(MinicParser::CallExprContext* ctx) -> std::any override;
  // auto visitParenExpr(MinicParser::ParenExprContext *ctx) -> std::any
  // override;
  auto visitBitwiseXorExpr(MinicParser::BitwiseXorExprContext* ctx)
      -> std::any override;
  auto visitAddSubExpr(MinicParser::AddSubExprContext* ctx)
      -> std::any override;
  auto visitVarList(MinicParser::VarListContext* ctx) -> std::any override;

  // auto visitExpr(MinicParser::ExprContext *ctx) -> std::any override;
  // auto visitAssignment(MinicParser::AssignmentContext *ctx)
  //     -> std::any override;

  // auto visitEquality(MinicParser::EqualityContext *ctx) -> std::any
  // override;

  // auto visitComparison(MinicParser::ComparisonContext *ctx)
  //     -> std::any override;

  // auto visitTerm(MinicParser::TermContext *ctx) -> std::any override;

  // auto visitFactor(MinicParser::FactorContext *ctx) -> std::any override;

  // auto visitUnary(MinicParser::UnaryContext *ctx) -> std::any override;

  // // auto visitPrimary(MinicParser::PrimaryContext *ctx) -> std::any
  // override;

  // auto visitParenExpr(MinicParser::ParenExprContext *ctx)
  // -> std::any override;
};

using InitDeclarator =
    std::pair<std::unique_ptr<Declarator>, std::unique_ptr<Initializer>>;

}  // namespace Minic