#include "AST.hh"
#include "Context.hh"
#include "Log.hh"
#include "Type.hh"
#include "Visitor.hh"

#include <cassert>
#include <cstddef>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <vector>

namespace Minic {

/* =============================== CodeGenVisitor =========================== */
/* ================================== Private =============================== */
void CodeGenVisitor::checkVariableRedefinition(
    const std::unique_ptr<Declarator> &D) {
  auto *Ret = Current->FindCurrent(D->getName());
  if (Ret) {
    panic("Redefinition of " + D->getName());
  }
}

void CodeGenVisitor::checkVariableRedefinition(
    const std::vector<std::unique_ptr<Declarator>> &DList) {
  for (const auto &D : DList) {
    checkVariableRedefinition(D);
  }
}

/* =============================== CodeGenVisitor =========================== */
auto CodeGenVisitor::Visit(const Program &TheProgram) -> void {
  for (const auto &Decl : TheProgram) {
    Visit(Decl.get());
  }
}

auto CodeGenVisitor::Visit(ASTNode *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(Declaration *Node) -> void { Node->accept(this); }

/// https://stackoverflow.com/questions/45471470/how-to-generate-code-for-initializing-global-variables-with-non-const-values-in
auto CodeGenVisitor::Visit(VarDecl *Node) -> void {
  // TODO : Check LiteralExpr Type `int x = 1.3;`
  // TODO : Handle Array
  /// DataType Identifier = Expr
  auto *Type = LW->getType(Node->getType());

  checkVariableRedefinition(Node->TheDeclaratorList);

  if (Current->isTop()) {
    for (size_t i = 0; i < Node->TheDeclaratorList.size(); ++i) {
      Constant *TheInitializer = nullptr;
      if (Node->TheInitializerList[i]) {
        // Have initializer
        TheValue = nullptr;
        Visit(Node->TheInitializerList[i].get());
        if (!TheValue) {
          panic("Failed to generate the initializer");
        }
        TheInitializer = static_cast<Constant *>(TheValue);
        if (!TheInitializer) {
          panic("Failed to generate the initializer, static_cast");
        }
      } else {
        TheInitializer = LW->getDefaultConstant(Type);
      }
      auto Name = Node->TheDeclaratorList[i]->Name;
      auto *GV = new GlobalVariable(*LW->Mod.get(), Type, false,
                                    llvm::GlobalValue::ExternalLinkage,
                                    TheInitializer, Name);
      Current->Add(Name, GV);
    }
    return;
  }

  auto *TheFunction = LW->Builder->GetInsertBlock()->getParent();

  for (size_t i = 0; i < Node->TheDeclaratorList.size(); ++i) {
    Constant *TheInitializer = nullptr;
    if (Node->TheInitializerList[i]) {
      // Have initializer
      TheValue = nullptr;
      Visit(Node->TheInitializerList[i].get());
      if (!TheValue) {
        panic("Failed to generate the initializer");
      }
      TheInitializer = static_cast<Constant *>(TheValue);
      if (!TheInitializer) {
        panic("Failed to generate the initializer, static_cast");
      }
    }
    auto Name = Node->TheDeclaratorList[i]->Name;
    auto *Alloca = LLVMWrapper::CreateEntryBlockAlloca(TheFunction, Type, Name);
    // LOL
    if (TheInitializer) {
      LW->Builder->CreateStore(TheInitializer, Alloca);
    }

    Current->Add(Name, Alloca);
  }
}

auto CodeGenVisitor::Visit(FunctionDecl *Node) -> void {
  if (!Current->isTop()) {
    panic("Nested function is not allowed");
    assert(0 && "UNREACHABLE");
    return;
  }

  auto *Ret = Current->Find(Node->Name);

  // Lookup
  if (Ret) {
    auto *F = static_cast<Function *>(Ret);
    if (!F) {
      panic(std::string(Ret->getName()) + "is defined and is not a function");
    }
    if (!F->empty()) {
      panic("Redefinition of Function " + std::string(Ret->getName()));
    }
  }

  // Prototype
  std::vector<llvm::Type *> TypeList;
  std::vector<std::string> NameList;
  auto *RetType = LW->getType(Node->Type);
  for (const auto &ParmVar : Node->VarList) {
    auto Type = ParmVar->getType();
    const auto &VarName = ParmVar->getName();
    TypeList.emplace_back(LW->getType(Type));
    NameList.emplace_back(VarName);
  }

  auto *FT = FunctionType::get(RetType, TypeList, false);
  auto *F = Function::Create(FT, Function::ExternalLinkage, Node->Name,
                             LW->Mod.get());
  for (unsigned i = 0, end = F->arg_size(); i < end; i++) {
    (F->args().begin() + i)->setName(NameList[i]);
  }

  if (Node->isPrototype()) {
    TheValue = F;
    Current->Add(Node->Name, F);
    return;
  }

  // Body
  auto *BB = BasicBlock::Create(*LW->Ctx.get(), "entry", F);
  LW->Builder->SetInsertPoint(BB);

  auto Child = std::make_unique<Scope>(Current);
  Current = Child.get();
  IsFunctionScope = true;

  for (auto &Arg : F->args()) {
    // Create an alloca for this variable
    auto *Alloca = LLVMWrapper::CreateEntryBlockAlloca(F, Arg.getType(),
                                                       Arg.getName().str());
    LW->Builder->CreateStore(&Arg, Alloca);
    Current->Add(std::string(Arg.getName()), Alloca);
  }

  TheValue = nullptr;
  Visit(Node->Body.get());
  // Finish off the function
  llvm::verifyFunction(*F);
  TheValue = F;

  Current->Add(Node->Name, F);
  return;
}

auto CodeGenVisitor::Visit(ParmVarDecl *Node) -> void { panic("Unreachable"); }

auto CodeGenVisitor::Visit(Expr *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(VariableExpr *Node) -> void {
  // TODO Handle Array
  const auto &VarName = Node->getName();
  auto *Val = Current->Find(VarName);
  if (!Val) {
    panic("Unknown Variable " + VarName);
  }
  auto *Casted = static_cast<llvm::AllocaInst *>(Val);
  if (!Casted) {
    panic(VarName + " is not a variable stored in stack");
  }

  // maybe redundant
  // TheValue = LW->Builder->CreateLoad(Casted->getAllocatedType(), Casted,
  //  VarName.c_str());
  TheValue = Casted;
}

auto CodeGenVisitor::Visit(CallExpr *Node) -> void {
  // TODO Check Args Type ???
  auto *Val = Current->Find(Node->Callee);
  auto *TheFunction = static_cast<Function *>(Val);
  if (!Val) {
    panic(Node->Callee + " is not function");
  }

  const auto &Args = Node->Args;
  // Check Size
  if (Args.size() != TheFunction->arg_size()) {
    panic("Wrong arguments size!!!");
  }

  // Type checking ???
  std::vector<llvm::Value *> ArgVs;
  for (const auto &Arg : Args) {
    TheValue = nullptr;
    Visit(Arg.get());
    if (!TheValue) {
      panic("Failed to generate " + Node->Callee + "'s args");
    }
    ArgVs.emplace_back(TheValue);
  }

  TheValue = LW->Builder->CreateCall(TheFunction, ArgVs, "calltmp");
}

auto CodeGenVisitor::Visit(UnaryExpr *Node) -> void {}

auto CodeGenVisitor::Visit(BinaryExpr *Node) -> void {
  auto BinOp = Node->TheBinaryOperator;

  TheValue = nullptr;
  Visit(Node->LHS.get());
  if (!TheValue) {
    panic("Failed to generate IR of LHS");
  }
  auto LHS = Addr;

  TheValue = nullptr;
  Visit(Node->RHS.get());
  if (!TheValue) {
    panic("Failed to generate IR of LHS");
  }
  auto RHS = TheValue;

  // '='
  // TODO Check LValue
  if (BinOp == BinaryOperator::Assign) {
    LW->Builder->CreateStore(RHS, LHS);
    return;
  }

  // cast
  // int -> float,  (1.1 + 4)
  //
  switch (BinOp) {
  case Minic::BinaryOperator::Assign:
    panic("'=' have been handled above");
    break;
  case Minic::BinaryOperator::Plus:
    break;
  case Minic::BinaryOperator::Minus:
    break;
  case Minic::BinaryOperator::Multiply:
    break;
  case Minic::BinaryOperator::Divide:
    break;
  case Minic::BinaryOperator::And:
    break;
  case Minic::BinaryOperator::Or:
    break;
  case Minic::BinaryOperator::Less:
    break;
  case Minic::BinaryOperator::LessEqual:
    break;
  case Minic::BinaryOperator::Greater:
    break;
  case Minic::BinaryOperator::GreaterEqual:
    break;
  case Minic::BinaryOperator::Equal:
    break;
  case Minic::BinaryOperator::NotEqual:
    break;
  }
}

auto CodeGenVisitor::Visit(LiteralExpr *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(LiteralIntegerExpr *Node) -> void {
  TheValue = ConstantInt::get(*LW->Ctx.get(), APInt(32, Node->Val));
}

auto CodeGenVisitor::Visit(LiteralFloatExpr *Node) -> void {
  TheValue = ConstantFP::get(*LW->Ctx.get(), APFloat(Node->Val));
}

auto CodeGenVisitor::Visit(LiteralCharExpr *Node) -> void {
  TheValue = ConstantInt::get(*LW->Ctx.get(), APInt(8, Node->Char));
}

auto CodeGenVisitor::Visit(Statement *Node) -> void {
  if (!Node) {
    return;
  }
  Node->accept(this);
}

auto CodeGenVisitor::Visit(ExprStmt *Node) -> void {
  Visit(Node->TheExpr.get());
}

auto CodeGenVisitor::Visit(IfStmt *Node) -> void {
  TheValue = nullptr;
  Visit(Node->Cond.get());
  if (!TheValue) {
    panic("Failed to generate if condition");
  }
  auto *CondV = TheValue;
  // Check LValue
  if (Node->Cond->isLValue()) {
    auto *Casted = llvm::dyn_cast<llvm::AllocaInst>(CondV);
    auto *Var = dynamic_cast<VariableExpr *>(Node->Cond.get());
    if (!Casted) {
      panic("just panic");
    }
    CondV = LW->Builder->CreateLoad(Casted->getAllocatedType(), Casted,
                                    Var->getName());
  }

  CondV = LW->convertToBool(CondV, "ifcond");
  if (!CondV) {
    panic("Failed to convert to bool");
  }

  auto *TheFunction = LW->Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  BasicBlock *ThenBB = BasicBlock::Create(*LW->Ctx, "then", TheFunction);
  BasicBlock *ElseBB = BasicBlock::Create(*LW->Ctx, "else");
  BasicBlock *MergeBB = BasicBlock::Create(*LW->Ctx, "ifcont");

  LW->Builder->CreateCondBr(CondV, ThenBB, ElseBB);

  // Emit then value.
  LW->Builder->SetInsertPoint(ThenBB);

  Visit(Node->IfBody.get());
  LW->Builder->CreateBr(MergeBB);

  // Emit else block.
  TheFunction->insert(TheFunction->end(), ElseBB);
  LW->Builder->SetInsertPoint(ElseBB);

  Visit(Node->ElseBody.get());
  LW->Builder->CreateBr(MergeBB);

  // Emit merge block.
  TheFunction->insert(TheFunction->end(), MergeBB);

  LW->Builder->SetInsertPoint(MergeBB);
}

auto CodeGenVisitor::Visit(WhileStmt *Node) -> void {}

auto CodeGenVisitor::Visit(ReturnStmt *Node) -> void {
  TheValue = nullptr;
  Visit(Node->Expression.get());
  if (!TheValue) {
    panic("Failed to generate return statement Expression");
  }
  LW->Builder->CreateRet(TheValue);
}

auto CodeGenVisitor::Visit(BreakStmt *Node) -> void {}

auto CodeGenVisitor::Visit(ContinueStmt *Node) -> void {}

auto CodeGenVisitor::Visit(VarDeclStmt *Node) -> void {
  Visit(Node->TheVarDecl.get());
}

auto CodeGenVisitor::Visit(CompoundStmt *Node) -> void {
  std::unique_ptr<Scope> Child = nullptr;
  if (!IsFunctionScope) {
    // IsFunctionScope false
    Child = std::make_unique<Scope>(Current);
    Current = Child.get();
  } else {
    IsFunctionScope = false;
  }

  for (const auto &Stmt : Node->Statements) {
    Visit(Stmt.get());
  }

  Current = Current->getParent();
}

auto CodeGenVisitor::Visit(Declarator *Node) -> void {}

auto CodeGenVisitor::Visit(Initializer *Node) -> void {
  if (!Node) {
    return;
  }
  // TODO Handle Array
  Visit(Node->TheExpr.get());
}

} // namespace Minic