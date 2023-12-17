#pragma once

#include "Type.hh"
#include "Visitor.hh"

#include <memory>
#include <string>
#include <utility>
#include <vector>
namespace Minic {

class ASTVisitor;

class ASTNode {
public:
  virtual ~ASTNode() = default;
  virtual auto accept(ASTVisitor *) -> void = 0;
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
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override;
};

class Initializer : ASTNode {
  bool _IsLeaf;
  std::unique_ptr<Expr> TheExpr; // nullptr if is not leaf node
  std::vector<std::unique_ptr<Initializer>> Children;

public:
  explicit Initializer(std::unique_ptr<Expr> TheExpr,
                       std::vector<std::unique_ptr<Initializer>> &&Children =
                           std::vector<std::unique_ptr<Initializer>>{})
      : _IsLeaf(Children.empty()), TheExpr(std::move(TheExpr)),
        Children(std::move(Children)) {}
  [[nodiscard]] auto IsLeaf() const -> bool { return _IsLeaf; }
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override;
};

/// Declaration
///   ::= VarDecl
///   ::= FunctionDecl
///   ::= ParmVarDecl
class Declaration : public ASTNode {
protected:
  DataType Type;

public:
  explicit Declaration(DataType Type) : Type(Type) {}
};

using DeclaratorList = std::vector<std::unique_ptr<Declarator>>;
using InitializerList = std::vector<std::unique_ptr<Initializer>>; // optional

/// Single Variable Declaration
///   ::= DataType Declarator
///   ::= DataType Declarator '=' Expr
class VarDecl : public Declaration {
protected:
  DeclaratorList TheDeclaratorList;
  InitializerList TheInitializerList; // optional

public:
  VarDecl(DataType Type, DeclaratorList TheDeclaratorList,
          InitializerList &&TheInitializerList = InitializerList{})
      : Declaration(Type), TheDeclaratorList(std::move(TheDeclaratorList)),
        TheInitializerList(std::move(TheInitializerList)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "VarDecl " + DataType2String[Type]; }
  [[nodiscard]] auto GetType() const -> DataType { return Type; }

  friend class ASTPrinter;
};

/// VariableExpr
///   ::= Identifier
class VariableExpr : public Expr {
  std::string Name;

public:
  explicit VariableExpr(std::string Name) : Name(std::move(Name)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "VariableExpr"; }
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
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "CallExpr"; }
};

/// BinayExpr
///   ::= Expr BinayOperator Expr
class BinaryExpr : public Expr {
  BinayOperator TheBinaryOperator;
  std::unique_ptr<Expr> LHS, RHS;

public:
  BinaryExpr(BinayOperator TheBinaryOperator, std::unique_ptr<Expr> LHS,
             std::unique_ptr<Expr> RHS)
      : TheBinaryOperator(TheBinaryOperator), LHS(std::move(LHS)),
        RHS(std::move(RHS)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "BinayExpr"; }
};

/// UnaryExpr
///   ::= UnaryOperator Expr
class UnaryExpr : public Expr {
  UnaryOperator TheUnaryOperator;
  std::unique_ptr<Expr> TheExpr;

public:
  UnaryExpr(UnaryOperator UnaryOperator, std::unique_ptr<Expr> Expression)
      : TheUnaryOperator(UnaryOperator), TheExpr(std::move(Expression)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "UnaryExpr"; }
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
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "LiteralIntegerExpr"; }
};

/// Literal Float Expression.
///   ::= [0-9]+\.[0-9]*
///   ::= \.[0-9]+
class LiteralFloatExpr : public LiteralExpr {
  float Val;

public:
  explicit LiteralFloatExpr(float Val) : Val(Val) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "LiteralFloatExpr"; }
};

class LiteralCharExpr : public LiteralExpr {
  char Char;

public:
  explicit LiteralCharExpr(char Char) : Char(Char) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "LiteralCharExpr"; }
};

/// Statement
///   ::= IfStmt
///   ::= WhileStmt
///   ::= ReturnStmt
///   ::= VarDeclStmt
///   ::= ReturnStmt
///   ::= ContinueStmt
class Statement : public ASTNode {};

/// Compound Statement
///   ::= CompoundStmt | Statement
class CompoundStmt : public ASTNode {
protected:
  std::vector<std::unique_ptr<Statement>> Statements;

public:
  explicit CompoundStmt(std::vector<std::unique_ptr<Statement>> Statements)
      : Statements(std::move(Statements)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "CompoudStmt"; }

  friend class ASTPrinter;
};

/// ExprStmt
///   ::= Expr ;
class ExprStmt : public Statement {
  std::unique_ptr<Expr> TheExpr;

public:
  explicit ExprStmt(std::unique_ptr<Expr> Expr) : TheExpr(std::move(Expr)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "ExprStmt"; }
};

/// If Statement else is optional.
///   ::= if ( Expr ) '{' CompoundStmt '}'
///   ::= if ( Expr ) '{' CompoundStmt '}' else '{' CompoundStmt '}'
class IfStmt : public Statement {
protected:
  std::unique_ptr<Expr> Cond;
  // Else Body could be nullptr because Else is optional
  std::unique_ptr<CompoundStmt> IfBody, ElseBody;

public:
  IfStmt(std::unique_ptr<Expr> Cond, std::unique_ptr<CompoundStmt> IfBody,
         std::unique_ptr<CompoundStmt> ElseBody = nullptr)
      : Cond(std::move(Cond)), IfBody(std::move(IfBody)),
        ElseBody(std::move(ElseBody)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "IfStmt"; }

  friend class ASTPrinter;
};

/// While Statement.
///   ::= 'while' '(' Expr ')' '{' CompoundStmt '}'
class WhileStmt : public Statement {
protected:
  std::unique_ptr<Expr> Cond;
  std::unique_ptr<CompoundStmt> Body;

public:
  WhileStmt(std::unique_ptr<Expr> Cond, std::unique_ptr<CompoundStmt> Body)
      : Cond(std::move(Cond)), Body(std::move(Body)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "WhileStmt"; }

  friend class ASTPrinter;
};

/// Return Statement
///   ::= 'return' Expr;
class ReturnStmt : public Statement {
  std::unique_ptr<Expr> Expression;

public:
  explicit ReturnStmt(std::unique_ptr<Expr> Expression)
      : Expression(std::move(Expression)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "ReturnStmt"; }
};

class BreakStmt : public Statement {
public:
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "BreakStmt"; }
};

class ContinueStmt : public Statement {
public:
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "ContinueStmt"; }
};

class VarDeclStmt : public Statement {
protected:
  std::unique_ptr<VarDecl> TheVarDecl;

public:
  explicit VarDeclStmt(std::unique_ptr<VarDecl> TheVarDecl)
      : TheVarDecl(std::move(TheVarDecl)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "VarDeclStmt"; }

  friend class ASTPrinter;
};

/// Global Scope
class GlobalVarDecl : public Declaration {
protected:
  std::unique_ptr<VarDecl> TheVarDecl;

public:
  explicit GlobalVarDecl(DataType Type, std::unique_ptr<VarDecl> TheVarDecl)
      : Declaration(Type), TheVarDecl(std::move(TheVarDecl)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "GlobalVarDecl"; }

  friend class ASTPrinter;
};

/// ParmVarDecl
///     ::= DataType Declarator
class ParmVarDecl : public Declaration {
  std::unique_ptr<Declarator> TheDeclarator;

public:
  ParmVarDecl(DataType Type, std::unique_ptr<Declarator> TheDeclarator)
      : Declaration(Type), TheDeclarator(std::move(TheDeclarator)){};
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "ParmVarDecl"; }
};

using ParmVarList = std::vector<std::unique_ptr<ParmVarDecl>>;

/// Function Definition
///   ::= ReturnType Name '(' VarList ')' '{' CompoundStmt '}'
class FunctionDecl : public Declaration {
protected:
  std::string Name;
  ParmVarList VarList;
  std::unique_ptr<CompoundStmt> Body;

public:
  FunctionDecl(DataType ReturnType, std::string Name, ParmVarList VarList,
               std::unique_ptr<CompoundStmt> Body)
      : Declaration(ReturnType), Name(std::move(Name)),
        VarList(std::move(VarList)), Body(std::move(Body)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "FunctionDecl"; }

  friend class ASTPrinter;
};

using Program = std::vector<std::unique_ptr<Declaration>>;

} // namespace Minic