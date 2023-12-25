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
class Expr : public ASTNode {
protected:
  bool IsLValue = false;

public:
  [[nodiscard]] auto isLValue() const -> bool { return IsLValue; }
  auto setLValue(bool isLValue) -> void { IsLValue = isLValue; }
};

class Declarator : public ASTNode {
  bool IsArray = false;

protected:
  std::string Name;
  std::vector<int> Dimension{}; // when is not array, it should be empty

public:
  explicit Declarator(std::string Name) : Name(std::move(Name)) {}
  Declarator(std::string Name, std::vector<int> &&Dimension)
      : IsArray(true), Name(std::move(Name)), Dimension(Dimension) {}
  [[nodiscard]] auto isArray() const -> bool { return IsArray; }
  [[nodiscard]] auto getName() const -> const std::string & { return Name; }
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override;

  friend class CodeGenVisitor;
};

class Initializer : ASTNode {
  bool IsLeaf;
  std::unique_ptr<Expr> TheExpr; // nullptr if is not leaf node
  std::vector<std::unique_ptr<Initializer>> Children;

public:
  explicit Initializer(std::unique_ptr<Expr> TheExpr,
                       std::vector<std::unique_ptr<Initializer>> &&Children =
                           std::vector<std::unique_ptr<Initializer>>{})
      : IsLeaf(Children.empty()), TheExpr(std::move(TheExpr)),
        Children(std::move(Children)) {}
  [[nodiscard]] auto isLeaf() const -> bool { return IsLeaf; }
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override;

  friend class CodeGenVisitor;
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
  explicit operator std::string() override {
    return "VarDecl " + DataType2String[Type];
  }
  [[nodiscard]] auto getType() const -> DataType { return Type; }

  friend class ASTPrinter;
  friend class CodeGenVisitor;
};

/// VariableExpr
///   ::= Identifier
class VariableExpr : public Expr {
  std::string Name;

public:
  explicit VariableExpr(std::string Name) : Name(std::move(Name)) {}

  [[nodiscard]] auto getName() const -> const std::string & { return Name; }
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "VariableExpr"; }
};

/// CallExpr
///   ::= Identifier '(' Args ')'
/// Args
///   ::= Expr, Args
///   ::= <Empty>
class CallExpr : public Expr {
protected:
  std::string Callee;
  std::vector<std::unique_ptr<Expr>> Args;

public:
  CallExpr(std::string Callee, std::vector<std::unique_ptr<Expr>> Args)
      : Callee(std::move(Callee)), Args(std::move(Args)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "CallExpr"; }

  friend class CodeGenVisitor;
};

/// ArraySubscriptExpr
///   ::= Identifier ('[' expr ']')*
class ArraySubscriptExpr : public Expr {
protected:
  std::string Name;
  std::vector<std::unique_ptr<Expr>> Index;

public:
  ArraySubscriptExpr(std::string Name,
                     std::vector<std::unique_ptr<Expr>> &&Index)
      : Name(std::move(Name)), Index(std::move(Index)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override;
  auto getName() const -> const std::string & { return Name; }

  friend class ASTPrinter;
  friend class CodeGenVisitor;
};

/// BinayExpr
///   ::= Expr BinayOperator Expr
class BinaryExpr : public Expr {
protected:
  BinaryOperator TheBinaryOperator;
  std::unique_ptr<Expr> LHS, RHS;

public:
  BinaryExpr(BinaryOperator TheBinaryOperator, std::unique_ptr<Expr> LHS,
             std::unique_ptr<Expr> RHS)
      : TheBinaryOperator(TheBinaryOperator), LHS(std::move(LHS)),
        RHS(std::move(RHS)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "BinayExpr"; }

  friend class CodeGenVisitor;
};

/// UnaryExpr
///   ::= UnaryOperator Expr
class UnaryExpr : public Expr {
protected:
  UnaryOperator TheUnaryOperator;
  std::unique_ptr<Expr> TheExpr;

public:
  UnaryExpr(UnaryOperator UnaryOperator, std::unique_ptr<Expr> Expression)
      : TheUnaryOperator(UnaryOperator), TheExpr(std::move(Expression)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "UnaryExpr"; }

  friend class CodeGenVisitor;
};

/// Literal Expression
///   ::= LiteralInteger
///   ::= LiteralFloat
class LiteralExpr : public Expr {};

/// Literal Integer Expression.
///   ::= [0-9]+
class LiteralIntegerExpr : public LiteralExpr {
protected:
  int Val;

public:
  explicit LiteralIntegerExpr(int Val) : Val(Val) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "LiteralIntegerExpr"; }

  friend class CodeGenVisitor;
};

/// Literal Float Expression.
///   ::= [0-9]+\.[0-9]*
///   ::= \.[0-9]+
class LiteralFloatExpr : public LiteralExpr {
protected:
  float Val;

public:
  explicit LiteralFloatExpr(float Val) : Val(Val) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "LiteralFloatExpr"; }

  friend class CodeGenVisitor;
};

class LiteralCharExpr : public LiteralExpr {
protected:
  char Char;

public:
  explicit LiteralCharExpr(char Char) : Char(Char) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "LiteralCharExpr"; }

  friend class CodeGenVisitor;
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
class CompoundStmt : public Statement {
protected:
  std::vector<std::unique_ptr<Statement>> Statements;

public:
  explicit CompoundStmt(std::vector<std::unique_ptr<Statement>> Statements)
      : Statements(std::move(Statements)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "CompoudStmt"; }

  friend class ASTPrinter;
  friend class CodeGenVisitor;
};

/// ExprStmt
///   ::= Expr ;
class ExprStmt : public Statement {
protected:
  std::unique_ptr<Expr> TheExpr;

public:
  explicit ExprStmt(std::unique_ptr<Expr> Expr) : TheExpr(std::move(Expr)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "ExprStmt"; }

  friend class ASTPrinter;
  friend class CodeGenVisitor;
};

/// If Statement else is optional.
///   ::= if ( Expr ) '{' CompoundStmt '}'
///   ::= if ( Expr ) '{' CompoundStmt '}' else '{' CompoundStmt '}'
class IfStmt : public Statement {
protected:
  std::unique_ptr<Expr> Cond;
  // Else Body could be nullptr because Else is optional
  std::unique_ptr<Statement> IfBody, ElseBody;

public:
  IfStmt(std::unique_ptr<Expr> Cond, std::unique_ptr<Statement> IfBody,
         std::unique_ptr<Statement> ElseBody = nullptr)
      : Cond(std::move(Cond)), IfBody(std::move(IfBody)),
        ElseBody(std::move(ElseBody)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "IfStmt"; }

  friend class ASTPrinter;
  friend class CodeGenVisitor;
};

/// While Statement.
///   ::= 'while' '(' Expr ')' '{' CompoundStmt '}'
class WhileStmt : public Statement {
protected:
  std::unique_ptr<Expr> Cond;
  std::unique_ptr<Statement> Body;

public:
  WhileStmt(std::unique_ptr<Expr> Cond, std::unique_ptr<Statement> Body)
      : Cond(std::move(Cond)), Body(std::move(Body)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "WhileStmt"; }

  friend class ASTPrinter;
};

/// Return Statement
///   ::= 'return' Expr;
class ReturnStmt : public Statement {
  std::unique_ptr<Expr> TheExpr;

public:
  explicit ReturnStmt(std::unique_ptr<Expr> TheExpr = nullptr)
      : TheExpr(std::move(TheExpr)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "ReturnStmt"; }

  friend class CodeGenVisitor;
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
  friend class CodeGenVisitor;
};

/// ParmVarDecl
///     ::= DataType Declarator
class ParmVarDecl : public Declaration {
protected:
  std::unique_ptr<Declarator> TheDeclarator;

public:
  ParmVarDecl(DataType Type, std::unique_ptr<Declarator> TheDeclarator)
      : Declaration(Type), TheDeclarator(std::move(TheDeclarator)){};
  [[nodiscard]] auto getType() const -> DataType { return Type; }
  [[nodiscard]] auto getName() const -> const std::string & {
    return TheDeclarator->getName();
  }
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
  [[nodiscard]] auto isPrototype() const -> bool { return Body == nullptr; }
  explicit operator std::string() override;

  friend class ASTPrinter;
  friend class CodeGenVisitor;
};

using Program = std::vector<std::unique_ptr<Declaration>>;

} // namespace Minic