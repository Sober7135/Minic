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

class Declarator;

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

class Declarator : public ASTNode {
  std::string Name;

public:
  explicit Declarator(std::string Name) : Name(std::move(Name)) {}
};

class ArrayDeclarator : public Declarator {
  std::vector<int> Dimension; // int a[5][6]; the Dimension is [5, 6]
  int len;

public:
  ArrayDeclarator(std::string Name, std::vector<int> Dimension)
      : Declarator(std::move(Name)), Dimension(std::move(Dimension)) {
    len = Dimension.size();
  }
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

class SimpleDeclarator : public Declarator {
public:
  explicit SimpleDeclarator(std::string Name) : Declarator(std::move(Name)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// Declaration
///   ::= VarDecl
///   ::= ParamVarDecl
class Declaration : public ASTNode {
  DataType Type;
  std::unique_ptr<Declarator> TheDeclarator;

public:
  explicit Declaration(DataType Type, std::unique_ptr<Declarator> TheDeclarator)
      : Type(Type), TheDeclarator(std::move(TheDeclarator)) {}
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
///   ::= DataType Declarator
///   ::= DataType Declarator '=' Expr
class VarDecl : public Declaration {
  std::unique_ptr<Expr> Val; // optional

public:
  VarDecl(DataType Type, std::unique_ptr<Declarator> TheDeclarator,
          std::unique_ptr<Expr> Val = nullptr)
      : Declaration(Type, std::move(TheDeclarator)), Val(std::move(Val)) {}
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

/// ParmVarDecl
///     ::= DataType Declarator
class ParmVarDecl : public Declaration {
public:
  ParmVarDecl(DataType Type, std::unique_ptr<Declarator> TheDeclarator)
      : Declaration(Type, std::move(TheDeclarator)){};
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};

/// Function Declaration
///   ::= ReturnType Name '(' VarList ')' '{' CompoundStmt '}'
class FunctionDecl : public ASTNode {
  DataType ReturnType;
  std::string Name;
  ParmVarList VarList;
  CompoundStmt Body;

public:
  FunctionDecl(DataType ReturnType, std::string Name, ParmVarList VarList,
               CompoundStmt Body)
      : ReturnType(ReturnType), Name(std::move(Name)),
        VarList(std::move(VarList)), Body(std::move(Body)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
};
