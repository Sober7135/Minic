#pragma once

#include "Type.hh"
#include "Visitor.hh"

#include <llvm/IR/Type.h>
#include <memory>
#include <string>
#include <utility>
#include <vector>
namespace Minic {

class ASTVisitor;

class ASTNode {
public:
  enum Kind {
    FunctionDeclK,
    VarDeclK,
    ParmVarDeclK,
    /// Expr
    LiteralIntegerExprK,
    LiteralCharExprK,
    LiteralFloatExprK,
    BinaryExprK,
    UnaryExprK,
    ArraySubscriptExprK,
    CallExprK,
    VariableExprK,
    /// Statement
    CompoundStmtK,
    ExprStmtK,
    IfStmtK,
    WhileStmtK,
    ReturnStmtK,
    BreakStmtK,
    ContinueStmtK,
    VarDeclStmtK,
    DeclaratorK,
    InitializerK,
  };

  ASTNode(Kind Kind) : NodeK(Kind) {}
  virtual ~ASTNode() = default;
  virtual auto accept(ASTVisitor *) -> void = 0;
  virtual explicit operator std::string() = 0;
  [[nodiscard]] auto getKind() const -> Kind { return NodeK; }

private:
  Kind NodeK;
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
  explicit Expr(Kind Kind) : ASTNode(Kind) {}
  [[nodiscard]] auto isLValue() const -> bool { return IsLValue; }
  virtual auto setIsLValue(bool isLValue) -> void { IsLValue = isLValue; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() >= LiteralCharExprK &&
           Node->getKind() <= VariableExprK;
  }
};

class Declarator : public ASTNode {
  bool IsArray = false;

protected:
  std::string Name;
  std::vector<int> Dimension{}; // when is not array, it should be empty

public:
  explicit Declarator(std::string Name)
      : ASTNode(Kind::DeclaratorK), Name(std::move(Name)) {}
  Declarator(std::string Name, std::vector<int> &&Dimension)
      : ASTNode(Kind::DeclaratorK), IsArray(true), Name(std::move(Name)),
        Dimension(Dimension) {}
  [[nodiscard]] auto isArray() const -> bool { return IsArray; }
  [[nodiscard]] auto getName() const -> const std::string & { return Name; }
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::DeclaratorK;
  }
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
      : ASTNode(Kind::InitializerK), IsLeaf(Children.empty()),
        TheExpr(std::move(TheExpr)), Children(std::move(Children)) {}
  [[nodiscard]] auto isLeaf() const -> bool { return IsLeaf; }
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override;
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::InitializerK;
  }
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
  explicit Declaration(DataType Type, Kind Kind) : ASTNode(Kind), Type(Type) {}
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() >= FunctionDeclK && Node->getKind() <= ParmVarDeclK;
  }
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
      : Declaration(Type, Kind::VarDeclK),
        TheDeclaratorList(std::move(TheDeclaratorList)),
        TheInitializerList(std::move(TheInitializerList)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override {
    return "VarDecl " + DataType2String[Type];
  }
  [[nodiscard]] auto getType() const -> DataType { return Type; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::VarDeclK;
  }
  friend class ASTPrinter;
  friend class CodeGenVisitor;
};

/// VariableExpr
///   ::= Identifier
class VariableExpr : public Expr {
  std::string Name;

public:
  explicit VariableExpr(std::string Name)
      : Expr(Kind::VariableExprK), Name(std::move(Name)) {}

  [[nodiscard]] auto getName() const -> const std::string & { return Name; }
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "VariableExpr"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::VariableExprK;
  }
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
      : Expr(Kind::CallExprK), Callee(std::move(Callee)),
        Args(std::move(Args)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "CallExpr"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::CallExprK;
  }
  friend class CodeGenVisitor;
};

/// ArraySubscriptExpr
///   ::= Identifier ('[' expr ']')*
class ArraySubscriptExpr : public Expr {
protected:
  std::unique_ptr<Expr> Base;
  std::unique_ptr<Expr> Selector;

public:
  ArraySubscriptExpr(std::unique_ptr<Expr> Base,
                     std::unique_ptr<Expr> &&Selector)
      : Expr(Kind::ArraySubscriptExprK), Base(std::move(Base)),
        Selector(std::move(Selector)) {
    // By default, the outer is rvalue, inner are all lvalue
    IsLValue = false;
  }
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override;
  auto setIsLValue(bool isLValue) -> void override {
    this->IsLValue = isLValue;
  }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::ArraySubscriptExprK;
  }

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
      : Expr(Kind ::BinaryExprK), TheBinaryOperator(TheBinaryOperator),
        LHS(std::move(LHS)), RHS(std::move(RHS)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "BinayExpr"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::BinaryExprK;
  }
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
      : Expr(Kind::UnaryExprK), TheUnaryOperator(UnaryOperator),
        TheExpr(std::move(Expression)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "UnaryExpr"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::UnaryExprK;
  }
  friend class CodeGenVisitor;
};

/// Literal Expression
///   ::= LiteralInteger
///   ::= LiteralFloat
class LiteralExpr : public Expr {
public:
  explicit LiteralExpr(Kind Kind) : Expr(Kind) {}
};

/// Literal Integer Expression.
///   ::= [0-9]+
class LiteralIntegerExpr : public LiteralExpr {
protected:
  int Val;

public:
  explicit LiteralIntegerExpr(int Val)
      : LiteralExpr(Kind::LiteralIntegerExprK), Val(Val) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "LiteralIntegerExpr"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::LiteralIntegerExprK;
  }
  friend class CodeGenVisitor;
};

/// Literal Float Expression.
///   ::= [0-9]+\.[0-9]*
///   ::= \.[0-9]+
class LiteralFloatExpr : public LiteralExpr {
protected:
  float Val;

public:
  explicit LiteralFloatExpr(float Val)
      : LiteralExpr(Kind::LiteralFloatExprK), Val(Val) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "LiteralFloatExpr"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::LiteralFloatExprK;
  }
  friend class CodeGenVisitor;
};

class LiteralCharExpr : public LiteralExpr {
protected:
  char Char;

public:
  explicit LiteralCharExpr(char Char)
      : LiteralExpr(Kind::LiteralCharExprK), Char(Char) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "LiteralCharExpr"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::LiteralCharExprK;
  }
  friend class CodeGenVisitor;
};

/// Statement
///   ::= IfStmt
///   ::= WhileStmt
///   ::= ReturnStmt
///   ::= VarDeclStmt
///   ::= ReturnStmt
///   ::= ContinueStmt
class Statement : public ASTNode {
public:
  explicit Statement(Kind Kind) : ASTNode(Kind) {}
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() >= CompoundStmtK && Node->getKind() <= VarDeclStmtK;
  }
};

/// Compound Statement
///   ::= CompoundStmt | Statement
class CompoundStmt : public Statement {
protected:
  std::vector<std::unique_ptr<Statement>> Statements;

public:
  explicit CompoundStmt(std::vector<std::unique_ptr<Statement>> Statements)
      : Statement(Kind::CompoundStmtK), Statements(std::move(Statements)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "CompoudStmt"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::CompoundStmtK;
  }
  friend class ASTPrinter;
  friend class CodeGenVisitor;
};

/// ExprStmt
///   ::= Expr ;
class ExprStmt : public Statement {
protected:
  std::unique_ptr<Expr> TheExpr;

public:
  explicit ExprStmt(std::unique_ptr<Expr> Expr)
      : Statement(Kind::ExprStmtK), TheExpr(std::move(Expr)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "ExprStmt"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::ExprStmtK;
  }
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
      : Statement(Kind::IfStmtK), Cond(std::move(Cond)),
        IfBody(std::move(IfBody)), ElseBody(std::move(ElseBody)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "IfStmt"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::IfStmtK;
  }
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
      : Statement(Kind::WhileStmtK), Cond(std::move(Cond)),
        Body(std::move(Body)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "WhileStmt"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::WhileStmtK;
  }

  friend class ASTPrinter;
  friend class CodeGenVisitor;
};

/// Return Statement
///   ::= 'return' Expr;
class ReturnStmt : public Statement {
  std::unique_ptr<Expr> TheExpr;

public:
  explicit ReturnStmt(std::unique_ptr<Expr> TheExpr = nullptr)
      : Statement(Kind::ReturnStmtK), TheExpr(std::move(TheExpr)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "ReturnStmt"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::ReturnStmtK;
  }
  friend class CodeGenVisitor;
};

class BreakStmt : public Statement {
public:
  BreakStmt() : Statement(Kind::BreakStmtK) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "BreakStmt"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::BreakStmtK;
  }
};

class ContinueStmt : public Statement {
public:
  ContinueStmt() : Statement(Kind::ContinueStmtK) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "ContinueStmt"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::ContinueStmtK;
  }
};

class VarDeclStmt : public Statement {
protected:
  std::unique_ptr<VarDecl> TheVarDecl;

public:
  explicit VarDeclStmt(std::unique_ptr<VarDecl> TheVarDecl)
      : Statement(Kind::VarDeclStmtK), TheVarDecl(std::move(TheVarDecl)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "VarDeclStmt"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::VarDeclStmtK;
  }
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
      : Declaration(Type, Kind::ParmVarDeclK),
        TheDeclarator(std::move(TheDeclarator)){};
  [[nodiscard]] auto getType() const -> DataType { return Type; }
  [[nodiscard]] auto getName() const -> const std::string & {
    return TheDeclarator->getName();
  }
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  explicit operator std::string() override { return "ParmVarDecl"; }
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::ParmVarDeclK;
  }
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
      : Declaration(ReturnType, Kind::FunctionDeclK), Name(std::move(Name)),
        VarList(std::move(VarList)), Body(std::move(Body)) {}
  auto accept(ASTVisitor *V) -> void override { V->Visit(this); }
  [[nodiscard]] auto isPrototype() const -> bool { return Body == nullptr; }
  explicit operator std::string() override;
  static auto classof(const ASTNode *Node) -> bool {
    return Node->getKind() == Kind::FunctionDeclK;
  }
  friend class ASTPrinter;
  friend class CodeGenVisitor;
};

using Program = std::vector<std::unique_ptr<Declaration>>;

} // namespace Minic