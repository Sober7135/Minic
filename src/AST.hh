#pragma once

#include "TokenType.hh"
#include "Type.hh"
#include "Visitor.hh"

#include <format>
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
class LiteralCharExpr;

class Statement;
class ExprStmt;
class IfStmt;
class WhileStmt;
class ReturnStmt;
class BreakStmt;
class ContinueStmt;
class CompoundStmt;

class Declarator;

using ParmVarList = std::vector<std::unique_ptr<ParmVarDecl>>;
using Program = std::vector<std::unique_ptr<Declaration>>;

class ASTNode {
public:
  virtual ~ASTNode() = default;
  virtual auto accept(ASTVisitor &) -> void = 0;
  virtual explicit operator std::string() = 0;
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
  std::vector<int> Dimension{}; // when is not array, it should be empty
  bool _IsArray = false;

public:
  explicit Declarator(std::string Name) : Name(std::move(Name)) {}
  Declarator(std::string Name, std::vector<int> &&Dimension)
      : Name(std::move(Name)), Dimension(Dimension), _IsArray(true) {}
  [[nodiscard]] auto IsArray() const -> bool { return _IsArray; }
  auto accept(ASTVisitor &V) -> void override { V.visit(this); }
  explicit operator std::string() override { return std::format("Declarator"); }
};

class Initializer : ASTNode {
  std::unique_ptr<Expr> TheExpr; // nullptr if is not leaf node
  std::vector<std::unique_ptr<Initializer>> Children;
  bool _IsLeaf;

public:
  Initializer(std::unique_ptr<Expr> TheExpr,
              std::vector<std::unique_ptr<Initializer>> &&Children =
                  std::vector<std::unique_ptr<Initializer>>{})
      : TheExpr(std::move(TheExpr)), Children(std::move(Children)) {
    _IsLeaf = Children.empty();
  }
  [[nodiscard]] auto IsLeaf() const -> bool { return _IsLeaf; }
  auto accept(ASTVisitor &V) -> void override { V.visit(this); }
  explicit operator std::string() override {
    return std::format("Initializer");
  }
};

/// Declaration
///   ::= VarDecl
///   ::= ParamVarDecl
///   ::= FunctionDecl
class Declaration : public ASTNode {
  DataType Type;

public:
  explicit Declaration(DataType Type) : Type(Type) {}
};

/// Variable Declaration
///   ::= DataType Declarator
///   ::= DataType Declarator '=' Expr
class VarDecl : public Declaration {
  std::unique_ptr<Declarator> TheDeclarator;
  std::unique_ptr<Initializer> TheInitializer; // optional

public:
  VarDecl(DataType Type, std::unique_ptr<Declarator> TheDeclarator,
          std::unique_ptr<Initializer> TheInitializer = nullptr)
      : Declaration(Type), TheDeclarator(std::move(TheDeclarator)),
        TheInitializer(std::move(TheInitializer)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }

  explicit operator std::string() override { return std::format("VarDecl"); }
};

/// VariableExpr
///   ::= Identifier
class VariableExpr : public Expr {
  std::string Name;

public:
  explicit VariableExpr(std::string Name) : Name(std::move(Name)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override {
    return std::format("VariableExpr");
  }
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
  explicit operator std::string() override { return std::format("CallExpr"); }
};

/// BinayExpr
///   ::= Expr BinayOperator Expr
class BinayExpr : public Expr {
  BinayOperator TheBinaryOperator;
  std::unique_ptr<Expr> LHS, RHS;

public:
  BinayExpr(BinayOperator TheBinaryOperator, std::unique_ptr<Expr> LHS,
            std::unique_ptr<Expr> RHS)
      : TheBinaryOperator(TheBinaryOperator), LHS(std::move(LHS)),
        RHS(std::move(RHS)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override { return std::format("BinayExpr"); }
};

/// UnaryExpr
///   ::= UnaryOperator Expr
class UnaryExpr : public Expr {
  UnaryOperator TheUnaryOperator;
  std::unique_ptr<Expr> TheExpr;

public:
  UnaryExpr(UnaryOperator UnaryOperator, std::unique_ptr<Expr> Expression)
      : TheUnaryOperator(UnaryOperator), TheExpr(std::move(Expression)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override { return std::format("UnaryExpr"); }
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
  explicit operator std::string() override {
    return std::format("LiteralIntegerExpr");
  }
};

/// Literal Float Expression.
///   ::= [0-9]+\.[0-9]*
///   ::= \.[0-9]+
class LiteralFloatExpr : public LiteralExpr {
  float Val;

public:
  explicit LiteralFloatExpr(float Val) : Val(Val) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override {
    return std::format("LiteralFloatExpr");
  }
};

class LiteralCharExpr : public LiteralExpr {
  char Char;

public:
  explicit LiteralCharExpr(char Char) : Char(Char) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override {
    return std::format("LiteralCharExpr");
  }
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
  explicit operator std::string() override { return "CompoudStmt"; }
};

/// ExprStmt
///   ::= Expr ;
class ExprStmt : public Statement {
  std::unique_ptr<Expr> TheExpr;

public:
  explicit ExprStmt(std::unique_ptr<Expr> Expr) : TheExpr(std::move(Expr)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override { return std::format("ExprStmt"); }
};

/// If Statement else is optional.
///   ::= if ( Expr ) '{' CompoundStmt '}'
///   ::= if ( Expr ) '{' CompoundStmt '}' else '{' CompoundStmt '}'
class IfStmt : public Statement {
  std::unique_ptr<Expr> Cond;
  // Else Body could be nullptr because Else is optional
  std::unique_ptr<CompoundStmt> IfBody, ElseBody;

public:
  IfStmt(std::unique_ptr<Expr> Cond, std::unique_ptr<CompoundStmt> IfBody,
         std::unique_ptr<CompoundStmt> ElseBody = nullptr)
      : Cond(std::move(Cond)), IfBody(std::move(IfBody)),
        ElseBody(std::move(ElseBody)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override { return std::format("IfStmt"); }
};

/// While Statement.
///   ::= 'while' '(' Expr ')' '{' CompoundStmt '}'
class WhileStmt : public Statement {
  std::unique_ptr<Expr> Cond;
  std::unique_ptr<CompoundStmt> Body;

public:
  WhileStmt(std::unique_ptr<Expr> Cond, std::unique_ptr<CompoundStmt> Body)
      : Cond(std::move(Cond)), Body(std::move(Body)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override { return std::format("WhileStmt"); }
};

/// Return Statement
///   ::= 'return' Expr;
class ReturnStmt : public Statement {
  std::unique_ptr<Expr> Expression;

public:
  explicit ReturnStmt(std::unique_ptr<Expr> Expression)
      : Expression(std::move(Expression)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override { return std::format("ReturnStmt"); }
};

class BreakStmt : public Statement {
public:
  auto accept(ASTVisitor &V) -> void override { V.visit(this); }
  explicit operator std::string() override { return std::format("BreakStmt"); }
};

class ContinueStmt : public Statement {
public:
  auto accept(ASTVisitor &V) -> void override { V.visit(this); }
  explicit operator std::string() override {
    return std::format("ContinueStmt");
  }
};

/// ParmVarDecl
///     ::= DataType Declarator
class ParmVarDecl : public Declaration {
  std::unique_ptr<Declarator> TheDeclarator;

public:
  ParmVarDecl(DataType Type, std::unique_ptr<Declarator> TheDeclarator)
      : Declaration(Type), TheDeclarator(std::move(TheDeclarator)){};
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override {
    return std::format("ParmVarDecl");
  }
};

/// Function Definition
///   ::= ReturnType Name '(' VarList ')' '{' CompoundStmt '}'
class FunctionDecl : public Declaration {
  std::string Name;
  ParmVarList VarList;
  std::unique_ptr<CompoundStmt> Body;

public:
  FunctionDecl(DataType ReturnType, std::string Name, ParmVarList VarList,
               std::unique_ptr<CompoundStmt> Body)
      : Declaration(ReturnType), Name(std::move(Name)),
        VarList(std::move(VarList)), Body(std::move(Body)) {}
  auto accept(ASTVisitor &v) -> void override { v.visit(this); }
  explicit operator std::string() override {
    return std::format("{} {}", "int", Name);
  }
};
