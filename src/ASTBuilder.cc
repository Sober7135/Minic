#include "ASTBuilder.hh"
#include "AST.hh"
#include "MinicParser.h"
#include "Type.hh"
#include "tree/TerminalNode.h"

#include <any>
#include <cassert>
#include <cstddef>
#include <memory>
#include <string>
#include <utility>
#include <vector>
namespace Minic {

static Program TheProgramTemp;
static std::unique_ptr<Expr> VisitedExpr;
static std::unique_ptr<CompoundStmt> VisitedCompoundStmt;
static std::unique_ptr<Statement> VisitedStmt;
static std::vector<std::unique_ptr<InitDeclarator>> VisitedInitDeclaratorList;
static std::unique_ptr<InitDeclarator> VisitedInitDeclarator;
static std::unique_ptr<Declarator> VisitedDeclarator;
static std::unique_ptr<Initializer> VisitedInitializer;
static ParmVarList VisitedParmVarList;
static std::unique_ptr<ParmVarDecl> VisitedParmVarDecl;
static std::unique_ptr<VarDecl> VisitedVarDecl;
static std::vector<std::unique_ptr<Expr>> VisitedVarList;
static DataType VisitedType;

auto ASTBuilder::Build() -> void {
  visitProgram(TheProgramContext);
  TheProgram = std::move(TheProgramTemp);
}

/// VisitedStmt
auto ASTBuilder::visitVarDeclStmt(MinicParser::VarDeclStmtContext *ctx)
    -> std::any {
  visitVarDecl(ctx->varDecl());
  VisitedStmt = std::make_unique<VarDeclStmt>(std::move(VisitedVarDecl));
  return nullptr;
}

auto ASTBuilder::visitGlobalVarDeclStmt(
    MinicParser::GlobalVarDeclStmtContext *ctx) -> std::any {
  // Get VisitedVarDecl
  visitVarDecl(ctx->varDecl());

  TheProgramTemp.emplace_back(std::make_unique<GlobalVarDecl>(
      VisitedVarDecl->GetType(), std::move(VisitedVarDecl)));
  return nullptr;
}

/// VisitedVarDecl
auto ASTBuilder::visitVarDecl(MinicParser::VarDeclContext *ctx) -> std::any {
  // This could be in global scope or CompoundStmt
  // DataType Declarator Initializer

  // Get DataType, VisitedDataType
  visitDataType(ctx->dataType());

  // Get InitDeclaratorList, VisitedInitDeclaratorList
  visitInitDeclaratorList(ctx->initDeclaratorList());

  DeclaratorList TheDeclaratorList{};
  InitializerList TheInitializerList{};

  for (auto &&TheInitDeclarator : VisitedInitDeclaratorList) {
    TheDeclaratorList.emplace_back(std::move(TheInitDeclarator->first));
    TheInitializerList.emplace_back(std::move(TheInitDeclarator->second));
  }
  VisitedVarDecl = std::make_unique<VarDecl>(
      VisitedType, std::move(TheDeclaratorList), std::move(TheInitializerList));

  return nullptr;
}

/// Return: `DataType`
auto ASTBuilder::visitDataType(MinicParser::DataTypeContext *ctx) -> std::any {
  auto StringType = dynamic_cast<antlr4::tree::TerminalNode *>(ctx->children[0])
                        ->getSymbol()
                        ->getText();
  VisitedType = String2DataType[StringType];

  return nullptr;
}

/// VisitedInitDeclaratorList
auto ASTBuilder::visitInitDeclaratorList(
    MinicParser::InitDeclaratorListContext *ctx) -> std::any {
  std::vector<std::unique_ptr<InitDeclarator>> InitDeclaratorList;
  for (auto &TheInitDeclaratorCtx : ctx->initDeclarator()) {
    // Get InitDeclarator, VisitedInitDeclarator
    visitInitDeclarator(TheInitDeclaratorCtx);
    InitDeclaratorList.emplace_back(std::move(VisitedInitDeclarator));
  }
  VisitedInitDeclaratorList = std::move(InitDeclaratorList);

  return nullptr;
}

/// VisitedInitDelclarator
auto ASTBuilder::visitInitDeclarator(MinicParser::InitDeclaratorContext *ctx)
    -> std::any {
  // Get VisitedDeclarator
  visitDeclarator(ctx->declarator());
  // Get VisitedInitializer
  visitInitializer(ctx->initializer());

  VisitedInitDeclarator = std::make_unique<InitDeclarator>(std::make_pair<>(
      std::move(VisitedDeclarator), std::move(VisitedInitializer)));

  return nullptr;
}

// VisitedDeclarator
auto ASTBuilder::visitDeclarator(MinicParser::DeclaratorContext *ctx)
    -> std::any {
  auto Name = ctx->Identifier()->getText();

  if (ctx->LiteralInt().empty()) {
    VisitedDeclarator = std::make_unique<Declarator>(Name);
  } else {
    std::vector<int> Dimension;
    for (auto &TheLiteralInt : ctx->LiteralInt()) {
      Dimension.emplace_back(std::stoi(TheLiteralInt->getText()));
    }
    VisitedDeclarator =
        std::make_unique<Declarator>(Name, std::move(Dimension));
  }

  return nullptr;
}

/// VisitedInitializer
auto ASTBuilder::visitInitializer(MinicParser::InitializerContext *ctx)
    -> std::any {
  if (!ctx) {
    // No initializer
    VisitedInitializer = nullptr;

  } else if (ctx->initializer().empty()) {
    // Has no children, it is a Expr
    // Get VistedExpr
    visitExpr(ctx->expr());
    VisitedInitializer = std::make_unique<Initializer>(std::move(VisitedExpr));

  } else {
    std::vector<std::unique_ptr<Initializer>> TheInitializer;

    for (auto &TheInitializerCtx : ctx->initializer()) {
      // Get VistedInitializer
      visitInitializer(TheInitializerCtx);
      TheInitializer.emplace_back(std::move(VisitedInitializer));
    }

    VisitedInitializer =
        std::make_unique<Initializer>(nullptr, std::move(TheInitializer));
  }
  return nullptr;
}

/// VistedExpr
auto ASTBuilder::visitLiteral(MinicParser::LiteralContext *ctx) -> std::any {
  if (ctx->LiteralInt()) {
    VisitedExpr = std::make_unique<LiteralIntegerExpr>(
        std::stoi(ctx->LiteralInt()->getText()));
    return nullptr;
  } else if (ctx->LiteralFloat()) {
    VisitedExpr = std::make_unique<LiteralFloatExpr>(
        std::stof(ctx->LiteralFloat()->getText()));
    return nullptr;
  } else {
    VisitedExpr = std::make_unique<LiteralCharExpr>(
        (char)(ctx->LiteralFloat()->getText()[0]));
    return nullptr;
  }
  assert(0 && "Unknown Literal Type");
}

/// VisitedDeclaration
auto ASTBuilder::visitFunctionDecl(MinicParser::FunctionDeclContext *ctx)
    -> std::any {
  // DataType Name ParmVarList CompoundStmt
  // Get VisitedType
  visitDataType(ctx->dataType());
  auto TheDataType = VisitedType;

  // Get Callee
  auto Name = ctx->Identifier()->getText();

  // Get  TODO
  visitParmVarList(ctx->parmVarList());
  auto TheParmVarList = std::move(VisitedParmVarList);

  // Get VisitedCompoundStmt
  visitCompoundStmt(ctx->compoundStmt());
  auto TheCompoundStmt = std::move(VisitedCompoundStmt);

  auto TheFunction = std::make_unique<FunctionDecl>(
      TheDataType, Name, std::move(TheParmVarList), std::move(TheCompoundStmt));
  TheProgramTemp.emplace_back(std::move(TheFunction));

  return nullptr;
}

/// VistedParmVarList
auto ASTBuilder::visitParmVarList(MinicParser::ParmVarListContext *ctx)
    -> std::any {
  // ParmVarList could be empty
  if (!ctx) {
    VisitedParmVarList = ParmVarList{};
  } else {

    ParmVarList TheParmVarList;
    for (auto &TheParmVarDeclCtx : ctx->parmVarDecl()) {
      // Get VisitedParmVarDecl
      visitParmVarDecl(TheParmVarDeclCtx);
      TheParmVarList.emplace_back(std::move(VisitedParmVarDecl));
    }
    VisitedParmVarList = std::move(TheParmVarList);
  }

  return nullptr;
}

/// VisitedParmVarDecl
auto ASTBuilder::visitParmVarDecl(MinicParser::ParmVarDeclContext *ctx)
    -> std::any {
  // Get VisitedType
  visitDataType(ctx->dataType());
  // Get VistedDeclarator
  visitDeclarator(ctx->declarator());

  VisitedParmVarDecl =
      std::make_unique<ParmVarDecl>(VisitedType, std::move(VisitedDeclarator));

  return nullptr;
}

/// VisitedCompoundStmt
auto ASTBuilder::visitCompoundStmt(MinicParser::CompoundStmtContext *ctx)
    -> std::any {
  std::vector<std::unique_ptr<Statement>> TheStmts;
  for (auto &TheStmtCtx : ctx->statement()) {
    // Get VisitedStmt
    visitStatement(TheStmtCtx);
    TheStmts.emplace_back(std::move(VisitedStmt));
  }
  VisitedCompoundStmt = std::make_unique<CompoundStmt>(std::move(TheStmts));

  return nullptr;
}

/// VisitedStmt
auto ASTBuilder::visitIfStmt(MinicParser::IfStmtContext *ctx) -> std::any {
  // Cond IfBody ElseBody
  visitExpr(ctx->expr());
  auto Cond = std::move(VisitedExpr);

  std::unique_ptr<CompoundStmt> IfBody = nullptr, ElseBody = nullptr;

  // Get VisitedCompoundStmt
  visitCompoundStmt(ctx->compoundStmt(0));
  IfBody = std::move(VisitedCompoundStmt);

  if (ctx->compoundStmt().size() == 2) {
    // Has `else`
    // Get VisitedCompoundStmt
    visitCompoundStmt(ctx->compoundStmt(1));
    ElseBody = std::move(VisitedCompoundStmt);
  }
  VisitedStmt = std::make_unique<IfStmt>(std::move(Cond), std::move(IfBody),
                                         std::move(ElseBody));
  return nullptr;
}

/// VisitedStmt
auto ASTBuilder::visitWhileStmt(MinicParser::WhileStmtContext *ctx)
    -> std::any {
  // Get VisitedExpr
  visitExpr(ctx->expr());
  auto Cond = std::move(VisitedExpr);

  // Get VisitedCompoundStmt
  visitCompoundStmt(ctx->compoundStmt());
  auto Body = std::move(VisitedCompoundStmt);

  VisitedStmt = std::make_unique<WhileStmt>(std::move(Cond), std::move(Body));

  return nullptr;
}

// auto ASTBuilder::visitForStmt(MinicParser::ForStmtContext *ctx)
//     -> std::any {

//   return nullptr;
// }

/// VisitedStmt
auto ASTBuilder::visitExprStmt(MinicParser::ExprStmtContext *ctx) -> std::any {
  // Get VisitedExpr
  visitExpr(ctx->expr());
  VisitedStmt = std::make_unique<ExprStmt>(std::move(VisitedExpr));

  return nullptr;
}

/// VisitedStmt
auto ASTBuilder::visitReturnStmt(MinicParser::ReturnStmtContext *ctx)
    -> std::any {
  // Get VisitedExpr
  visitExpr(ctx->expr());
  VisitedStmt = std::make_unique<ReturnStmt>(std::move(VisitedExpr));

  return nullptr;
}

/// VisitedStmt
auto ASTBuilder::visitBreakStmt(MinicParser::BreakStmtContext *ctx)
    -> std::any {
  VisitedStmt = std::make_unique<BreakStmt>();
  return nullptr;
}

/// VisitedStmt
auto ASTBuilder::visitContinueStmt(MinicParser::ContinueStmtContext *ctx)
    -> std::any {
  VisitedStmt = std::make_unique<ContinueStmt>();
  return nullptr;
}

/// Return: `std::unique_ptr<Expr>`
// auto ASTBuilder::visitExpr(MinicParser::ExprContext *ctx) -> std::any
// {
//   return nullptr;
// }

/// VisitedExpr
auto ASTBuilder::visitEquality(MinicParser::EqualityContext *ctx) -> std::any {
  // size must >= 1
  auto size = ctx->children.size();
  assert(size % 2 == 1);
  size_t i = 0;
  auto *TheComparisonCtx =
      dynamic_cast<MinicParser::ComparisonContext *>(ctx->children[i++]);

  // Get VisitedExpr
  visitComparison(TheComparisonCtx);
  std::unique_ptr<Expr> LHS = std::move(VisitedExpr);

  while (i < size) {
    // Get binary operator
    auto StringBinaryOperator =
        dynamic_cast<antlr4::tree::TerminalNode *>(ctx->children[i++])
            ->getText();
    auto TheBinaryOperator = String2BinaryOperator[StringBinaryOperator];

    // Get RHS
    TheComparisonCtx =
        dynamic_cast<MinicParser::ComparisonContext *>(ctx->children[i++]);
    visitComparison(TheComparisonCtx);
    std::unique_ptr<Expr> RHS = std::move(VisitedExpr);

    LHS = std::make_unique<BinaryExpr>(TheBinaryOperator, std::move(LHS),
                                       std::move(RHS));
  }
  VisitedExpr = std::move(LHS);

  return nullptr;
}

/// VisitedExpr
auto ASTBuilder::visitComparison(MinicParser::ComparisonContext *ctx)
    -> std::any {
  // size must >= 1
  auto size = ctx->children.size();
  assert(size % 2 == 1);
  size_t i = 0;
  auto *TheTermCtx =
      dynamic_cast<MinicParser::TermContext *>(ctx->children[i++]);

  // Store in VisitedExpr
  visitTerm(TheTermCtx);
  std::unique_ptr<Expr> LHS = std::move(VisitedExpr);

  while (i < size) {
    // Get binary operator
    auto StringBinaryOperator =
        dynamic_cast<antlr4::tree::TerminalNode *>(ctx->children[i++])
            ->getText();
    auto TheBinaryOperator = String2BinaryOperator[StringBinaryOperator];

    TheTermCtx = dynamic_cast<MinicParser::TermContext *>(ctx->children[i++]);
    // Store in VisitedExpr
    visitTerm(TheTermCtx);
    std::unique_ptr<Expr> RHS = std::move(VisitedExpr);
    LHS = std::make_unique<BinaryExpr>(TheBinaryOperator, std::move(LHS),
                                       std::move(RHS));
  }

  VisitedExpr = std::move(LHS);
  return nullptr;
}

auto ASTBuilder::visitTerm(MinicParser::TermContext *ctx) -> std::any {
  // size must >= 1
  auto size = ctx->children.size();
  assert(size % 2 == 1);
  size_t i = 0;
  auto *TheFactorCtx =
      dynamic_cast<MinicParser::FactorContext *>(ctx->children[i++]);

  // Get VisitedExpr
  visitFactor(TheFactorCtx);
  std::unique_ptr<Expr> LHS = std::move(VisitedExpr);

  while (i < size) {
    // Get binary operator
    auto StringBinaryOperator =
        dynamic_cast<antlr4::tree::TerminalNode *>(ctx->children[i++])
            ->getText();
    auto TheBinaryOperator = String2BinaryOperator[StringBinaryOperator];

    // Get RHS
    TheFactorCtx =
        dynamic_cast<MinicParser::FactorContext *>(ctx->children[i++]);
    visitFactor(TheFactorCtx);
    std::unique_ptr<Expr> RHS = std::move(VisitedExpr);

    LHS = std::make_unique<BinaryExpr>(TheBinaryOperator, std::move(LHS),
                                       std::move(RHS));
  }
  VisitedExpr = std::move(LHS);

  return nullptr;
}

auto ASTBuilder::visitFactor(MinicParser::FactorContext *ctx) -> std::any {
  // size must >= 1
  auto size = ctx->children.size();
  assert(size % 2 == 1);
  size_t i = 0;
  auto *TheUnaryCtx =
      dynamic_cast<MinicParser::UnaryContext *>(ctx->children[i++]);

  // Get VisitedExpr
  visitUnary(TheUnaryCtx);
  std::unique_ptr<Expr> LHS = std::move(VisitedExpr);

  while (i < size) {
    // Get binary operator
    auto StringBinaryOperator =
        dynamic_cast<antlr4::tree::TerminalNode *>(ctx->children[i++])
            ->getText();
    auto TheBinaryOperator = String2BinaryOperator[StringBinaryOperator];

    // Get RHS
    TheUnaryCtx = dynamic_cast<MinicParser::UnaryContext *>(ctx->children[i++]);
    visitUnary(TheUnaryCtx);
    std::unique_ptr<Expr> RHS = std::move(VisitedExpr);
    LHS = std::make_unique<BinaryExpr>(TheBinaryOperator, std::move(LHS),
                                       std::move(RHS));
  }
  VisitedExpr = std::move(LHS);

  return nullptr;
}

/// VisitedExpr
auto ASTBuilder::visitUnary(MinicParser::UnaryContext *ctx) -> std::any {
  if (ctx->unary()) {
    // (Plus | Minus) unary
    auto Operator =
        dynamic_cast<antlr4::tree::TerminalNode *>(ctx->children[0])->getText();
    auto TheUnaryOperator = String2UnaryOperator[Operator];

    // Get VisitedExpr
    visitUnary(ctx->unary());
    auto TheUnaryExpr = std::move(VisitedExpr);
    VisitedExpr =
        std::make_unique<UnaryExpr>(TheUnaryOperator, std::move(TheUnaryExpr));
  } else {
    // primary
    // Get VisitedExpr
    visitPrimary(ctx->primary());
  }
  return nullptr;
}

// auto ASTBuilder::visitPrimary(MinicParser::PrimaryContext *ctx)
//     -> std::any {
//   if (!ctx->identifierExpr()) {
//     auto TheIdentifierExpr = std::move(
//         visitIdentifierExpr(ctx->identifierExpr()).as<std::unique_ptr<Expr>>());
//     return TheIdentifierExpr;
//   } else if (!ctx->literal()) {
//       auto
//   } else {
//     // parenExpr
//   }

//   assert(0 && "Fucking here");
// }

auto ASTBuilder::visitIdentifierExpr(MinicParser::IdentifierExprContext *ctx)
    -> std::any {
  if (ctx->callExpr() == nullptr) {
    // VariableExpr
    VisitedExpr = std::make_unique<VariableExpr>(ctx->Identifier()->getText());
  } else {
    // CallExpr
    // Get VisitedExpr
    visitCallExpr(ctx->callExpr());
  }

  return nullptr;
}

/// VisitedExpr
auto ASTBuilder::visitCallExpr(MinicParser::CallExprContext *ctx) -> std::any {
  auto Name = ctx->Identifier()->getText();
  std::vector<std::unique_ptr<Expr>> Args{};
  if (ctx->varList()) {
    // Get VisitedVarList
    visitVarList(ctx->varList());
    Args = std::move(VisitedVarList);
  }
  VisitedExpr = std::make_unique<CallExpr>(Name, std::move(Args));

  return nullptr;
}

// VisitedVarList
auto ASTBuilder::visitVarList(MinicParser::VarListContext *ctx) -> std::any {
  std::vector<std::unique_ptr<Expr>> TheVarList;
  for (auto &TheExprCtx : ctx->expr()) {
    // Get VisitedExpr
    visitExpr(TheExprCtx);
    TheVarList.emplace_back(std::move(VisitedExpr));
  }

  VisitedVarList = std::move(TheVarList);
  return nullptr;
}

// auto ASTBuilder::visitParenExpr(MinicParser::ParenExprContext *ctx)
//     -> std::any {

// }

} // namespace Minic