#include "AST.hh"
#include "Context.hh"
#include "Log.hh"
#include "Type.hh"
#include "Visitor.hh"
#include "Wrapper.hh"

#include <cassert>
#include <cstddef>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <string>
#include <vector>

namespace Minic {

/* =============================== CodeGenVisitor =========================== */
/* ================================== Private =============================== */
auto CodeGenVisitor::getValue(ASTNode *Node) -> llvm::Value * {
  TheValue = nullptr;
  Visit(Node);
  return TheValue;
}

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

auto CodeGenVisitor::visitPrototype(FunctionDecl *Node) -> llvm::Function * {
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

  auto *FT = llvm::FunctionType::get(RetType, TypeList, false);
  auto *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage,
                                   Node->Name, LW->Mod.get());
  for (unsigned i = 0, end = F->arg_size(); i < end; i++) {
    (F->args().begin() + i)->setName(NameList[i]);
  }
  return F;
}

auto CodeGenVisitor::visitGlobalVariable(llvm::Type *Type, VarDecl *Node)
    -> void {
  for (size_t i = 0; i < Node->TheDeclaratorList.size(); ++i) {
    llvm::Constant *TheInitializer = nullptr;
    auto *AllocaType = Type;

    if (Node->TheDeclaratorList[i]->isArray()) {
      if (Node->TheInitializerList[i]) {
        TheInitializer = visitArrayConstantInitilizer(
            Type, Node->TheInitializerList[i].get());
      } else {
        TheInitializer = llvm::ConstantAggregateZero::get(AllocaType);
      }
      AllocaType =
          LW->getArrayType(Type, Node->TheDeclaratorList[i]->Dimension);
    } else {
      if (Node->TheInitializerList[i]) {
        // Have initializer
        auto *Val = getValue(Node->TheInitializerList[i].get());
        if (!Val) {
          panic("Failed to generate the initializer");
        }
        LW->implicitConvert(Val, Type);
        TheInitializer = static_cast<llvm::Constant *>(Val);
        if (!TheInitializer) {
          panic("Failed to generate the initializer, static_cast");
        }
      } else {
        TheInitializer = LW->getDefaultConstant(Type);
      }
    }

    auto Name = Node->TheDeclaratorList[i]->Name;
    auto *GV = new llvm::GlobalVariable(*LW->Mod.get(), AllocaType, false,
                                        llvm::GlobalValue::ExternalLinkage,
                                        TheInitializer, Name);
    Current->Add(Name, GV);
  }
}

auto CodeGenVisitor::visitLocalVariable(llvm::Type *Type, VarDecl *Node)
    -> void {
  auto *TheFunction = LW->Builder->GetInsertBlock()->getParent();
  auto *AllocaType = Type;

  for (size_t i = 0; i < Node->TheDeclaratorList.size(); ++i) {
    if (Node->TheDeclaratorList[i]->isArray()) {
      std::vector<llvm::Value *> TheInitializer;
      if (Node->TheInitializerList[i]) {
        // TODO Check size
        TheInitializer =
            visitArrayInitilizer(Type, Node->TheInitializerList[i].get());
      }
      AllocaType =
          LW->getArrayType(Type, Node->TheDeclaratorList[i]->Dimension);
      auto Name = Node->TheDeclaratorList[i]->Name;
      auto *Alloca =
          LLVMWrapper::CreateEntryBlockAlloca(TheFunction, AllocaType, Name);
      // LOL
      if (!TheInitializer.empty()) {
        // DO INIT
        auto D = Node->TheDeclaratorList[i]->Dimension;
        init(AllocaType, Alloca, TheInitializer,
             Node->TheDeclaratorList[i]->Dimension);
      }

      Current->Add(Name, Alloca);
      continue;
    }
    llvm::Value *TheInitializer = nullptr;
    if (Node->TheInitializerList[i]) {
      // Have initializer
      TheInitializer = getValue(Node->TheInitializerList[i].get());
      if (!TheInitializer) {
        panic("Failed to generate the initializer");
      }
      LW->implicitConvert(TheInitializer, Type);
    }

    auto Name = Node->TheDeclaratorList[i]->Name;
    auto *Alloca =
        LLVMWrapper::CreateEntryBlockAlloca(TheFunction, AllocaType, Name);
    // LOL
    if (TheInitializer) {
      LW->Builder->CreateStore(TheInitializer, Alloca);
    }

    Current->Add(Name, Alloca);
  }
}

auto CodeGenVisitor::visitArrayConstantInitilizer(llvm::Type *Type,
                                                  Initializer *Init)
    -> llvm::Constant * {
  // check size
  if (Init->isLeaf()) {
    auto *Val = getValue(Init->TheExpr.get());
    if (!llvm::isa<llvm::Constant>(Val)) {
      panic("not a constant expr");
    }
    LW->implicitConvert(Val, Type);
    return llvm::dyn_cast<llvm::Constant>(Val);
  }
  std::vector<llvm::Constant *> List;
  for (const auto &c : Init->Children) {
    auto *Val = visitArrayConstantInitilizer(Type, c.get());
    List.emplace_back(Val);
  }

  return llvm::ConstantArray::get(
      llvm::ArrayType::get(Type, Init->Children.size()), List);
}

auto CodeGenVisitor::visitArrayInitilizer(llvm::Type *Type, Initializer *Init)
    -> std::vector<llvm::Value *> {
  if (Init->isLeaf()) {
    auto *Val = getValue(Init->TheExpr.get());
    LW->implicitConvert(Val, Type);
    return std::vector<llvm::Value *>{Val};
  }

  std::vector<llvm::Value *> List;
  for (const auto &c : Init->Children) {
    auto Val = visitArrayInitilizer(Type, c.get());
    for (const auto &item : Val) {
      List.emplace_back(item);
    }
  }
  return List;
}

/// FIXME
auto CodeGenVisitor::getArrayValueType(ArraySubscriptExpr *Node)
    -> llvm::Type * {
  auto *Ptr = Node->Base.get();
  while (!VariableExpr::classof(Ptr)) {
    Ptr = dynamic_cast<ArraySubscriptExpr *>(Ptr)->Base.get();
  }
  auto VarName = dynamic_cast<VariableExpr *>(Ptr)->getName();
  auto *Ret = Current->Find(VarName);
  llvm::Type *Type = nullptr;
  if (llvm::isa<llvm::GlobalVariable>(Ret)) {
    auto GV = llvm::dyn_cast<llvm::GlobalVariable>(Ret);
    Type = GV->getValueType();
  } else if (llvm::isa<llvm::AllocaInst>(Ret)) {
    auto *Alloca = llvm::dyn_cast<llvm::AllocaInst>(Ret);
    Type = Alloca->getAllocatedType();
  }
  while (Type->isArrayTy()) {
    Type = Type->getArrayElementType();
  }
  return Type;
}

auto test(const std::vector<int> &Dimension, size_t index) -> std::vector<int> {
  std::vector<int> Ret(Dimension.size(), 0);
  for (int i = Dimension.size() - 1; i >= 0; i--) {
    Ret[i] = index % Dimension[i];
    index /= Dimension[i];
  }
  return Ret;
}

auto CodeGenVisitor::init(llvm::Type *Type, llvm::Value *Ptr,
                          const std::vector<llvm::Value *> &InitList,
                          const std::vector<int> &Dimension) const -> void {
  std::vector<int> v(Dimension.size(), 0);
  for (size_t i = 0, end = InitList.size(); i != end; ++i) {
    std::vector<llvm::Value *> idxList(Dimension.size() + 1);
    // TODO refactor
    idxList[0] = llvm::ConstantInt::getIntegerValue(LW->getType(DataType::Int),
                                                    llvm::APInt(32, 0));
    auto Ret = test(Dimension, i);
    for (size_t ii = 0, ee = Ret.size(); ii != ee; ii++) {
      idxList[ii + 1] = llvm::ConstantInt::getIntegerValue(
          LW->getType(DataType::Int), llvm::APInt(32, Ret[ii]));
    }

    auto Addr = LW->Builder->CreateInBoundsGEP(Type, Ptr, idxList, "arrayidx");
    LW->Builder->CreateStore(InitList[i], Addr);
  }
}

/* =============================== CodeGenVisitor =========================== */
/* =================================== Public =============================== */
auto CodeGenVisitor::Visit(const Program &TheProgram) -> void {
  for (const auto &Decl : TheProgram) {
    Visit(Decl.get());
  }
}

auto CodeGenVisitor::Visit(ASTNode *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(Declaration *Node) -> void { Node->accept(this); }

/// https://stackoverflow.com/questions/45471470/how-to-generate-code-for-initializing-global-variables-with-non-const-values-in
auto CodeGenVisitor::Visit(VarDecl *Node) -> void {
  checkVariableRedefinition(Node->TheDeclaratorList);

  // TODO : Handle Array
  /// DataType Identifier = Expr
  auto *Type = LW->getType(Node->getType());

  if (Current->isTop()) {
    visitGlobalVariable(Type, Node);
    return;
  }

  visitLocalVariable(Type, Node);
}

auto CodeGenVisitor::Visit(FunctionDecl *Node) -> void {
  if (!Current->isTop()) {
    panic("Nested function is not allowed");
    assert(0 && "UNREACHABLE");
    return;
  }

  auto *Ret = Current->Find(Node->Name);
  llvm::Function *F = nullptr;

  if (!Ret) {
    // Not defined
    // Generate Prototype
    F = visitPrototype(Node);
  } else {
    // Found.
    // 1. Only prototype is defined
    // 2. fully defined and current is declaration not a definition.
    // 3. Not a function
    F = static_cast<llvm::Function *>(Ret);
    if (!F) {
      // 3.
      panic(std::string(Ret->getName()) + "is defined and is not a function");
    }
    if (!(F->empty() || Node->isPrototype())) {
      // 2.
      panic("Redefinition of Function " + std::string(Ret->getName()));
    }
    // 1.
  }

  Current->Add(Node->Name, F);

  if (Node->isPrototype()) {
    TheValue = F;
    return;
  }

  // Body
  auto *BB = llvm::BasicBlock::Create(*LW->Ctx.get(), "entry", F);
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

  Visit(Node->Body.get());

  // check main
  if (F->getName() == "main") {
    if (F->getReturnType() != LW->getType(DataType::Int)) {
      panic("main's return type must be int");
    }
    if (!isa<llvm::ReturnInst>(F->back().back())) {
      LW->Builder->CreateRet(
          llvm::ConstantInt::get(*LW->Ctx, llvm::APInt(32, 0)));
    }
  }

  // Avoid double return
  if (F->getReturnType()->isVoidTy() &&
      (!isa<llvm::ReturnInst>(F->back().back()))) {
    LW->Builder->CreateRetVoid();
  }
  // Finish off the function
  llvm::verifyFunction(*F);
  TheValue = F;

  return;
}

auto CodeGenVisitor::Visit(ParmVarDecl *Node) -> void { panic("Unreachable"); }

auto CodeGenVisitor::Visit(Expr *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(VariableExpr *Node) -> void {
  const auto &VarName = Node->getName();
  auto *Val = Current->Find(VarName);
  if (!Val) {
    panic("Unknown Variable " + VarName);
  }
  if (!Node->isLValue()) {
    LW->load(Val);
  }
  TheValue = Val;
}

auto CodeGenVisitor::Visit(CallExpr *Node) -> void {
  auto *Val = Current->Find(Node->Callee);
  auto *TheFunction = static_cast<llvm::Function *>(Val);
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

  for (size_t i = 0, end = Args.size(); i != end; i++) {
    auto *DestTy = (TheFunction->args().begin() + i)->getType();
    auto *Val = getValue(Args[i].get());

    if (!Val) {
      panic("Failed to generate " + Node->Callee + "'s args");
    }
    LW->implicitConvert(Val, DestTy);
    ArgVs.emplace_back(Val);
  }

  TheValue = LW->Builder->CreateCall(TheFunction, ArgVs);
}

auto CodeGenVisitor::Visit(ArraySubscriptExpr *Node) -> void {
  static llvm::Type *Type = nullptr;
  if (!ArraySubscriptExpr::classof(Node->Base.get())) {
    if (!VariableExpr::classof(Node->Base.get())) {
      panic("just panic");
    }
    auto *Base = getValue(Node->Base.get());
    if (llvm::isa<llvm::GlobalVariable>(Base)) {
      auto GV = llvm::dyn_cast<llvm::GlobalVariable>(Base);
      Type = GV->getValueType();
    } else if (llvm::isa<llvm::AllocaInst>(Base)) {
      auto *Alloca = llvm::dyn_cast<llvm::AllocaInst>(Base);
      Type = Alloca->getAllocatedType();
    }
    auto *Selector = getValue(Node->Selector.get());
    TheValue = LW->Builder->CreateInBoundsGEP(
        Type, Base,
        std::vector<llvm::Value *>{
            llvm::ConstantInt::getIntegerValue(LW->getType(DataType::Int),
                                               llvm::APInt(32, 0)),
            Selector},
        "arrayidx");

    Type = Type->getArrayElementType();

    return;
  }
  auto *Base = getValue(Node->Base.get());
  auto *Selector = getValue(Node->Selector.get());
  TheValue = LW->Builder->CreateInBoundsGEP(
      Type, Base,
      std::vector<llvm::Value *>{
          llvm::ConstantInt::getIntegerValue(LW->getType(DataType::Int),
                                             llvm::APInt(32, 0)),
          Selector},
      "arrayidx");

  Type = Type->getArrayElementType();
  if (!Node->isLValue()) {
    TheValue = LW->Builder->CreateLoad(Type, TheValue);
  }
}

auto CodeGenVisitor::Visit(UnaryExpr *Node) -> void {
  auto UnaryOp = Node->TheUnaryOperator;
  auto *Expr = getValue(Node->TheExpr.get());
  auto *Ty = Expr->getType();
  if (!Expr) {
    panic("Failed to generate Expr in unaryExpr");
  }

  switch (UnaryOp) {
  case Minic::UnaryOperator::Plus:
    // skip
    break;
  case Minic::UnaryOperator::Minus:
    if (Ty->isFloatTy()) {
      TheValue = LW->Builder->CreateFSub(
          llvm::ConstantFP::get(Ty, llvm::APFloat(0.0)), Expr, "iunaryminus");
    } else {
      TheValue = LW->Builder->CreateSub(
          llvm::ConstantInt::getIntegerValue(Ty, llvm::APInt(32, 0)), Expr,
          "funaryminus");
    }
    break;
  case Minic::UnaryOperator::BitwiseNot:
    if (Ty->isFloatTy()) {
      panic("float is not allowed in bitwisenot");
    }
    TheValue = LW->Builder->CreateNot(Expr, "bitwisenot");
    break;
  case Minic::UnaryOperator::LogicalNot:
    LW->convertToBool(Expr);
    TheValue = LW->Builder->CreateXor(
        Expr, llvm::ConstantInt::getBool(*LW->Ctx, true), "logicalnot");
    break;
  default:
    panic("Unsupported unary operator");
  }
}

auto CodeGenVisitor::Visit(BinaryExpr *Node) -> void {
  auto BinOp = Node->TheBinaryOperator;

  auto *LHS = getValue(Node->LHS.get());
  if (!LHS) {
    panic("Failed to generate IR of LHS");
  }

  auto *RHS = getValue(Node->RHS.get());
  if (!RHS) {
    panic("Failed to generate IR of RHS");
  }

  // '='
  if (BinOp == BinaryOperator::Assign) {
    if (!Node->LHS->isLValue()) {
      panic("Assign to RValue is not allowed..");
    }

    auto *Ty = LW->getPtrType(LHS);
    if (!Ty) {
      Ty = getArrayValueType(
          dynamic_cast<ArraySubscriptExpr *>(Node->LHS.get()));
    }
    LW->implicitConvert(RHS, Ty);

    LW->Builder->CreateStore(RHS, LHS);

    // Value of assignment expression is RHS
    TheValue = RHS;
    return;
  }

  auto convert = [&](llvm::Value *&LHS, llvm::Value *&RHS) {
    if (LHS->getType()->isFloatTy() || RHS->getType()->isFloatTy()) {
      LW->implicitConvert(LHS, LW->getType(DataType::Float));
      LW->implicitConvert(RHS, LW->getType(DataType::Float));
      return true;
    }
    // Is Integer... Integer lift
    LW->implicitConvert(LHS, LW->getType(DataType::Int));
    LW->implicitConvert(RHS, LW->getType(DataType::Int));
    return false;
  };

  switch (BinOp) {
  case Minic::BinaryOperator::Assign:
    panic("'=' have been handled above");
    break;
  case Minic::BinaryOperator::Plus:
    if (convert(LHS, RHS)) {
      TheValue = LW->Builder->CreateFAdd(LHS, RHS, "faddtmp");
    } else {
      TheValue = LW->Builder->CreateAdd(LHS, RHS, "addtmp");
    }
    break;
  case Minic::BinaryOperator::Minus:
    if (convert(LHS, RHS)) {
      TheValue = LW->Builder->CreateFSub(LHS, RHS, "fsubtmp");
    } else {
      TheValue = LW->Builder->CreateSub(LHS, RHS, "subtmp");
    }
    break;
  case Minic::BinaryOperator::Multiply:
    if (convert(LHS, RHS)) {
      TheValue = LW->Builder->CreateFMul(LHS, RHS, "fmultmp");
    } else {
      TheValue = LW->Builder->CreateMul(LHS, RHS, "multmp");
    }
    break;
  case Minic::BinaryOperator::Divide:
    if (convert(LHS, RHS)) {
      TheValue = LW->Builder->CreateFDiv(LHS, RHS, "fdivtmp");
    } else {
      TheValue = LW->Builder->CreateSDiv(LHS, RHS, "divtmp");
    }
    break;
  case Minic::BinaryOperator::Precent:
    if (LHS->getType()->isFloatTy() || RHS->getType()->isFloatTy()) {
      panic("float is not allowed in mod");
    }
    TheValue = LW->Builder->CreateSRem(LHS, RHS, "reminder");
    break;
  case Minic::BinaryOperator::LogicalAnd:
    LW->convertToBool(LHS);
    LW->convertToBool(RHS);
    TheValue = LW->Builder->CreateLogicalAnd(LHS, RHS, "logicalandtmp");
    break;
  case Minic::BinaryOperator::LogicalOr:
    LW->convertToBool(LHS);
    LW->convertToBool(RHS);
    TheValue = LW->Builder->CreateLogicalOr(LHS, RHS, "logicalortmp");
    break;
  case Minic::BinaryOperator::Less:
    if (convert(LHS, RHS)) {
      TheValue = LW->Builder->CreateFCmpOLT(LHS, RHS, "flesstmp");
    } else {
      TheValue = LW->Builder->CreateICmpSLT(LHS, RHS, "lesstmp");
    }
    break;
  case Minic::BinaryOperator::LessEqual:
    if (convert(LHS, RHS)) {
      TheValue = LW->Builder->CreateFCmpOLE(LHS, RHS, "flesstmp");
    } else {
      TheValue = LW->Builder->CreateICmpSLE(LHS, RHS, "lesstmp");
    }
    break;
  case Minic::BinaryOperator::Greater:
    if (convert(LHS, RHS)) {
      TheValue = LW->Builder->CreateFCmpOGT(LHS, RHS, "flesstmp");
    } else {
      TheValue = LW->Builder->CreateICmpSGT(LHS, RHS, "lesstmp");
    }
    break;
  case Minic::BinaryOperator::GreaterEqual:
    if (convert(LHS, RHS)) {
      TheValue = LW->Builder->CreateFCmpOGE(LHS, RHS, "flesstmp");
    } else {
      TheValue = LW->Builder->CreateICmpSGE(LHS, RHS, "lesstmp");
    }
    break;
  case Minic::BinaryOperator::Equal:
    if (convert(LHS, RHS)) {
      TheValue = LW->Builder->CreateFCmpOEQ(LHS, RHS, "flesstmp");
    } else {
      TheValue = LW->Builder->CreateICmpEQ(LHS, RHS, "lesstmp");
    }
    break;
  case Minic::BinaryOperator::NotEqual:
    if (convert(LHS, RHS)) {
      TheValue = LW->Builder->CreateFCmpONE(LHS, RHS, "flesstmp");
    } else {
      TheValue = LW->Builder->CreateICmpNE(LHS, RHS, "lesstmp");
    }
    break;
  case Minic::BinaryOperator::BitwiseAnd:
    if (LHS->getType()->isFloatTy() || RHS->getType()->isFloatTy()) {
      panic("float is not allowed in &");
    }
    TheValue = LW->Builder->CreateAnd(LHS, RHS, "bitwiseand");
    break;
  case Minic::BinaryOperator::BitwiseOr:
    if (LHS->getType()->isFloatTy() || RHS->getType()->isFloatTy()) {
      panic("float is not allowed in |");
    }
    TheValue = LW->Builder->CreateOr(LHS, RHS, "bitwiseor");
    break;
  case Minic::BinaryOperator::BitwiseXor:
    if (LHS->getType()->isFloatTy() || RHS->getType()->isFloatTy()) {
      panic("float is not allowed in ^");
    }
    TheValue = LW->Builder->CreateXor(LHS, RHS, "xor");
    break;
  }
}

auto CodeGenVisitor::Visit(LiteralExpr *Node) -> void { Node->accept(this); }

auto CodeGenVisitor::Visit(LiteralIntegerExpr *Node) -> void {
  TheValue = llvm::ConstantInt::get(*LW->Ctx.get(), llvm::APInt(32, Node->Val));
}

auto CodeGenVisitor::Visit(LiteralFloatExpr *Node) -> void {
  TheValue = llvm::ConstantFP::get(*LW->Ctx.get(), llvm::APFloat(Node->Val));
}

auto CodeGenVisitor::Visit(LiteralCharExpr *Node) -> void {
  TheValue = llvm::ConstantInt::get(*LW->Ctx.get(), llvm::APInt(8, Node->Char));
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
  auto *CondV = getValue(Node->Cond.get());
  if (!CondV) {
    panic("Failed to generate if condition");
  }

  LW->convertToBool(CondV, "ifcond");
  if (!CondV) {
    panic("Failed to convert to bool");
  }

  auto *TheFunction = LW->Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function.
  auto *ThenBB = llvm::BasicBlock::Create(*LW->Ctx, "then", TheFunction);
  auto *ElseBB = llvm::BasicBlock::Create(*LW->Ctx, "else");
  auto *MergeBB = llvm::BasicBlock::Create(*LW->Ctx, "ifcont");

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

auto CodeGenVisitor::Visit(WhileStmt *Node) -> void {
  // TODO
}

auto CodeGenVisitor::Visit(ReturnStmt *Node) -> void {
  if (!Node->TheExpr) {
    LW->Builder->CreateRetVoid();
    return;
  }

  auto *RetExpr = getValue(Node->TheExpr.get());
  if (!RetExpr) {
    panic("Failed to generate return statement Expression");
  }
  LW->Builder->CreateRet(RetExpr);
}

auto CodeGenVisitor::Visit(BreakStmt *Node) -> void {
  // TODO
}

auto CodeGenVisitor::Visit(ContinueStmt *Node) -> void {
  // TODO
}

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

auto CodeGenVisitor::Visit([[maybe_unused]] Declarator *Node) -> void {
  panic("");
}

auto CodeGenVisitor::Visit(Initializer *Node) -> void {
  if (!Node) {
    return;
  }
  if (Node->isLeaf()) {
    Visit(Node->TheExpr.get());
    return;
  }
  panic("failed: array not handled here");
}

} // namespace Minic