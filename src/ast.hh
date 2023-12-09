#pragma once

#include "type.hh"
#include "visitor.hh"

#include <memory>
#include <string>
#include <utility>
#include <vector>

class ASTNode;

class Declaration;
class VarDecl;
class ParmVarDecl;
class FunctionDecl;

class Expr;
class VariableExpr;
class CallExpr;
class UnaryExpr;
class BinayExpr;
class LiteralExpr;
class LiteralIntegerExpr;
class LiteralFloatExpr;

class Statement;
class ExprStmt;
class DeclStmt;
class IfStmt;
class WhileStmt;
class ReturnStmt;

class CompoundStmt;

using ParmVarList = std::vector<std::unique_ptr<ParmVarDecl>>;
using TranslationUnit = std::vector<std::unique_ptr<Declaration>>;

class ASTNode {
public:
  virtual ~ASTNode() = default;
  virtual auto accept(ASTVisitor &) -> void = 0;
};

/// Expression.
///   ::= VariableExpr
///   ::= CallExpr
///   ::= BinaryExpr
///   ::= UnaryExpr
///   ::=
class Expr : public ASTNode {};

/// Declaration
///   ::= VarDecl
///   ::= ParamVarDecl
///   ::= FunctionDecl
class Declaration : public ASTNode {
protected:
  std::string Name;

public:
  [[nodiscard]] auto getName() const noexcept -> const std::string & {
    return Name;
  }
  explicit Declaration(std::string Name) : Name(std::move(Name)) {}
};

/// VariableExpr
///   ::= Identifier
class VariableExpr : public Expr {
  std::string Name;

public:
  explicit VariableExpr(std::string Name) : Name(std::move(Name)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// CallExpr
///   ::= Identifer '(' Args ')'
/// Args
///   ::= Expr, Args
///   ::= <Empty>
class CallExpr : public Expr {
  std::string Callee;
  std::vector<std::unique_ptr<Expr>> Args;

public:
  CallExpr(std::string Callee, std::vector<std::unique_ptr<Expr>> Args)
      : Callee(std::move(Callee)), Args(std::move(Args)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// BinayExpr
///   ::= Expr BinayOperator Expr
class BinayExpr : public Expr {
  std::string BinaryOperator;
  std::unique_ptr<Expr> LHS, RHS;

public:
  BinayExpr(std::string BinOp, std::unique_ptr<Expr> LHS,
            std::unique_ptr<Expr> RHS)
      : BinaryOperator(std::move(BinOp)), LHS(std::move(LHS)),
        RHS(std::move(RHS)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// UnaryExpr
///   ::= UnaryOperator Expr
class UnaryExpr : public Expr {
  std::string UnaryOperator;
  std::unique_ptr<Expr> Expression;

public:
  UnaryExpr(std::string UnaryOperator, std::unique_ptr<Expr> Expression)
      : UnaryOperator(std::move(UnaryOperator)),
        Expression(std::move(Expression)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// Literal Expression
///   ::= LiteralInteger
///   ::= LiteralFloat
class LiteralExpr : public Expr {};

/// Literal Integer Expression.
///   ::= [0-9]+
class LiteralIntegerExpr : public LiteralExpr {
  int Val;

public:
  explicit LiteralIntegerExpr(int Val) : Val(Val) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// Literal Float Expression.
///   ::= [0-9]+\.[0-9]*
///   ::= \.[0-9]+
class LiteralFloatExpr : public LiteralExpr {
  float Val;

public:
  explicit LiteralFloatExpr(float Val) : Val(Val) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// Statement
///   ::= IfStmt
///   ::= WhileStmt
///   ::= ReturnStmt
///   ::= DeclStmt
class Statement : public ASTNode {};

/// Compound Statement
///   ::= CompoundStmt | Statement
class CompoundStmt : public ASTNode {
  std::vector<std::unique_ptr<Statement>> Statements;

public:
  explicit CompoundStmt(std::vector<std::unique_ptr<Statement>> Statements)
      : Statements(std::move(Statements)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// ExprStmt
///   ::= Expr ;
class ExprStmt : public Statement {
  std::unique_ptr<Expr> Expression;

public:
  explicit ExprStmt(std::unique_ptr<Expr> Expr) : Expression(std::move(Expr)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// Variable Declaration
///   ::= DataType identifier
///   ::= DataType identifier '=' Expr
class VarDecl : public Declaration {
  DataType Type;
  std::unique_ptr<Expr> Val; // optional

public:
  VarDecl(DataType Type, std::string Name, std::unique_ptr<Expr> Val = nullptr)
      : Declaration(std::move(Name)), Type(Type), Val(std::move(Val)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// Declaration Statement, in Compound Statement
///   ::= VarDecl ;
class DeclStmt : public Statement { // TODO
  std::unique_ptr<VarDecl> VariableDeclaration;

public:
  explicit DeclStmt(std::unique_ptr<VarDecl> VariableDeclaration)
      : VariableDeclaration(std::move(VariableDeclaration)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// If Statement else is optional.
///   ::= if ( Expr ) '{' CompoundStmt '}'
///   ::= if ( Expr ) '{' CompoundStmt '}' else '{' CompoundStmt '}'
class IfStmt : public Statement {
  std::unique_ptr<Expr> Cond;
  // Else Body could be nullptr because Else is optional
  CompoundStmt IfBody, ElseBody;

public:
  IfStmt(std::unique_ptr<Expr> Cond, CompoundStmt IfBody, CompoundStmt ElseBody)
      : Cond(std::move(Cond)), IfBody(std::move(IfBody)),
        ElseBody(std::move(ElseBody)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// While Statement.
///   ::= 'while' '(' Expr ')' '{' CompoundStmt '}'
class WhileStmt : public Statement {
  std::unique_ptr<Expr> Cond;
  std::vector<std::unique_ptr<Statement>> Body;

public:
  WhileStmt(std::unique_ptr<Expr> Cond,
            std::vector<std::unique_ptr<Statement>> Body)
      : Cond(std::move(Cond)), Body(std::move(Body)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// Return Statement
///   ::= 'return' Expr;
class ReturnStmt : public Statement {
  std::unique_ptr<Expr> Expression;

public:
  explicit ReturnStmt(std::unique_ptr<Expr> Expression)
      : Expression(std::move(Expression)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// Parameter Variable Declaration
/// Part of VarDecl, not support default parameter value
class ParmVarDecl : public VarDecl {
public:
  ParmVarDecl(DataType Type, std::string Name)
      : VarDecl(Type, std::move(Name)){};
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// Function Declaration
///   ::= ReturnType Name '(' VarList ')' '{' CompoundStmt '}'
class FunctionDecl : public Declaration {
  DataType RetType;
  ParmVarList VarList;
  CompoundStmt Body;

public:
  FunctionDecl(DataType RetType, std::string Name, ParmVarList VarList,
               CompoundStmt Body)
      : Declaration(std::move(Name)), RetType(RetType),
        VarList(std::move(VarList)), Body(std::move(Body)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};
