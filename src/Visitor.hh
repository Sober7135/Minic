#pragma once

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Type.h>

#include <memory>
#include <system_error>
#include <vector>

#include "Context.hh"
#include "Wrapper.hh"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"

namespace Minic {

class ASTNode;

class Declaration;
class VarDecl;
class ParmVarDecl;
class FunctionDecl;

class Expr;
class VariableExpr;
class CallExpr;
class ArraySubscriptExpr;
class UnaryExpr;
class BinaryExpr;
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
class VarDeclStmt;

class CompoundStmt;
class Declarator;
class Initializer;

class ASTVisitor {
public:
  virtual auto Visit(ASTNode* Node) -> void = 0;

  virtual auto Visit(Declaration* Node) -> void = 0;
  virtual auto Visit(VarDecl* Node) -> void = 0;
  virtual auto Visit(ParmVarDecl* Node) -> void = 0;
  virtual auto Visit(FunctionDecl* Node) -> void = 0;

  virtual auto Visit(Expr* Node) -> void = 0;
  virtual auto Visit(VariableExpr* Node) -> void = 0;
  virtual auto Visit(CallExpr* Node) -> void = 0;
  virtual auto Visit(ArraySubscriptExpr* Node) -> void = 0;
  virtual auto Visit(UnaryExpr* Node) -> void = 0;
  virtual auto Visit(BinaryExpr* Node) -> void = 0;
  virtual auto Visit(LiteralExpr* Node) -> void = 0;
  virtual auto Visit(LiteralIntegerExpr* Node) -> void = 0;
  virtual auto Visit(LiteralFloatExpr* Node) -> void = 0;
  virtual auto Visit(LiteralCharExpr* Node) -> void = 0;

  virtual auto Visit(Statement* Node) -> void = 0;
  virtual auto Visit(ExprStmt* Node) -> void = 0;
  virtual auto Visit(IfStmt* Node) -> void = 0;
  virtual auto Visit(WhileStmt* Node) -> void = 0;
  virtual auto Visit(ReturnStmt* Node) -> void = 0;
  virtual auto Visit(BreakStmt* Node) -> void = 0;
  virtual auto Visit(ContinueStmt* Node) -> void = 0;
  virtual auto Visit(VarDeclStmt* Node) -> void = 0;

  virtual auto Visit(CompoundStmt* Node) -> void = 0;
  virtual auto Visit(Declarator* Node) -> void = 0;
  virtual auto Visit(Initializer* Node) -> void = 0;
};

class ASTPrinter: public ASTVisitor {
private:
  unsigned I {};  // Indent
  llvm::raw_fd_ostream Out;
  using Program = std::vector<std::unique_ptr<Declaration>>;

public:
  explicit ASTPrinter(const std::string& filename, std::error_code& EC) :
      Out(llvm::raw_fd_ostream(filename, EC)) {};

  auto Visit(const Program& TheProgram) -> void;

  auto Visit(ASTNode* Node) -> void override;
  auto Visit(Declaration* Node) -> void override;
  auto Visit(VarDecl* Node) -> void override;
  auto Visit(ParmVarDecl* Node) -> void override;
  auto Visit(FunctionDecl* Node) -> void override;

  auto Visit(Expr* Node) -> void override;
  auto Visit(VariableExpr* Node) -> void override;
  auto Visit(CallExpr* Node) -> void override;
  auto Visit(ArraySubscriptExpr* Node) -> void override;
  auto Visit(UnaryExpr* Node) -> void override;
  auto Visit(BinaryExpr* Node) -> void override;
  auto Visit(LiteralExpr* Node) -> void override;
  auto Visit(LiteralIntegerExpr* Node) -> void override;
  auto Visit(LiteralFloatExpr* Node) -> void override;
  auto Visit(LiteralCharExpr* Node) -> void override;

  auto Visit(Statement* Node) -> void override;
  auto Visit(ExprStmt* Node) -> void override;
  auto Visit(IfStmt* Node) -> void override;
  auto Visit(WhileStmt* Node) -> void override;
  auto Visit(ReturnStmt* Node) -> void override;
  auto Visit(BreakStmt* Node) -> void override;
  auto Visit(ContinueStmt* Node) -> void override;
  auto Visit(VarDeclStmt* Node) -> void override;

  auto Visit(CompoundStmt* Node) -> void override;
  auto Visit(Declarator* Node) -> void override;
  auto Visit(Initializer* Node) -> void override;
};

class CodeGenVisitor: public ASTVisitor {
  using Program = std::vector<std::unique_ptr<Declaration>>;

public:
  std::unique_ptr<LLVMWrapper> LW;

private:
  std::unique_ptr<Scope> TheScope;
  Scope* Current;
  llvm::Value* TheValue = nullptr;
  std::vector<llvm::Value*> TheInitializerList;
  bool IsInLoop = false;
  bool IsFunctionScope = false;
  llvm::BasicBlock* LoopCond = nullptr;
  llvm::BasicBlock* LoopEnd = nullptr;
  bool Terminate = false;

  auto getValue(ASTNode* Node) -> llvm::Value*;
  void checkVariableRedefinition(const std::unique_ptr<Declarator>& D);
  void checkVariableRedefinition(
      const std::vector<std::unique_ptr<Declarator>>& DList
  );
  auto visitPrototype(FunctionDecl* Node) -> llvm::Function*;
  auto visitGlobalVariable(llvm::Type* Type, VarDecl* Node) -> void;
  auto visitLocalVariable(llvm::Type* Type, VarDecl* Node) -> void;
  auto visitArrayConstantInitilizer(llvm::Type* Type, Initializer* Init)
      -> llvm::Constant*;
  auto visitArrayInitilizer(llvm::Type* Type, Initializer* Init)
      -> std::vector<llvm::Value*>;
  auto getArrayValueType(ArraySubscriptExpr* Node) -> llvm::Type*;
  auto init(
      llvm::Type* Type,
      llvm::Value* Ptr,
      const std::vector<llvm::Value*>& InitList,
      const std::vector<int>& Dimension
  ) const -> void;

public:
  explicit CodeGenVisitor(const std::string& ModuleID) :
      LW(std::make_unique<LLVMWrapper>(ModuleID)),
      TheScope(std::make_unique<Scope>()),
      Current(TheScope.get()) {}

  auto Visit(const Program& TheProgram) -> void;

  auto Visit(ASTNode* Node) -> void override;
  auto Visit(Declaration* Node) -> void override;
  auto Visit(VarDecl* Node) -> void override;
  auto Visit(ParmVarDecl* Node) -> void override;
  auto Visit(FunctionDecl* Node) -> void override;

  auto Visit(Expr* Node) -> void override;
  auto Visit(VariableExpr* Node) -> void override;
  auto Visit(CallExpr* Node) -> void override;
  auto Visit(ArraySubscriptExpr* Node) -> void override;
  auto Visit(UnaryExpr* Node) -> void override;
  auto Visit(BinaryExpr* Node) -> void override;
  auto Visit(LiteralExpr* Node) -> void override;
  auto Visit(LiteralIntegerExpr* Node) -> void override;
  auto Visit(LiteralFloatExpr* Node) -> void override;
  auto Visit(LiteralCharExpr* Node) -> void override;

  auto Visit(Statement* Node) -> void override;
  auto Visit(ExprStmt* Node) -> void override;
  auto Visit(IfStmt* Node) -> void override;
  auto Visit(WhileStmt* Node) -> void override;
  auto Visit(ReturnStmt* Node) -> void override;
  auto Visit(BreakStmt* Node) -> void override;
  auto Visit(ContinueStmt* Node) -> void override;
  auto Visit(VarDeclStmt* Node) -> void override;

  auto Visit(CompoundStmt* Node) -> void override;
  auto Visit(Declarator* Node) -> void override;
  auto Visit(Initializer* Node) -> void override;
};

}  // namespace Minic