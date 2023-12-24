#include "Wrapper.hh"
#include "Log.hh"
#include "Type.hh"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#include <ranges>
#include <vector>

namespace Minic {
auto LLVMWrapper::getType(DataType Type) -> llvm::Type * {
  switch (Type) {
  case DataType::Int:
    return llvm::Type::getInt32Ty(*Ctx);
  case DataType::Float:
    return llvm::Type::getFloatTy(*Ctx);
  case DataType::Char:
    return llvm::Type::getInt8Ty(*Ctx);
  case DataType::Void:
    return llvm::Type::getVoidTy(*Ctx);
  default:
    panic("Unsupport Type");
  }
}

auto LLVMWrapper::convertToBool(llvm::Value *&Val, std::string Name) -> void {

  auto Type = Val->getType();
  if (Type->isIntegerTy()) {
    auto W = Type->getIntegerBitWidth();
    Val = Builder->CreateICmpNE(
        Val,
        llvm::ConstantInt::get(llvm::IntegerType::get(*Ctx, W),
                               llvm::APInt(W, 0)),
        Name);
    return;
  } else if (Type->isFloatTy()) {
    Val = Builder->CreateFCmpONE(
        Val, llvm::ConstantFP::get(*Ctx, llvm::APFloat(0.0)), Name);
    return;
  }
  panic("Unknown conversion to bool");
}

auto LLVMWrapper::getDefaultConstant(llvm::Type *Type) -> llvm::Constant * {
  if (Type->isIntegerTy()) {
    auto W = Type->getIntegerBitWidth();
    return llvm::ConstantInt::get(*Ctx, llvm::APInt(W, 0));
  } else {
    return llvm::ConstantFP::get(*Ctx, llvm::APFloat(0.0));
  }
}

auto LLVMWrapper::implicitConvert(llvm::Value *&Val, llvm::Type *DestTy)
    -> void {
  auto ValType = Val->getType();
  if (ValType == DestTy) {
    return;
  }
  unsigned flag = 0;
  flag |= ValType->isFloatTy();
  flag |= ((unsigned)DestTy->isFloatTy() << (unsigned)1);

  // just Convert to LHS
  switch (flag) {
  case 0b00:
    // both are int
    Val = Builder->CreateIntCast(Val, DestTy, true, "inttoint");
    break;
  case 0b01:
    // Val is float
    // DestTy is integer
    Val = Builder->CreateFPToSI(Val, DestTy, "floattoint");
    break;
  case 0b10:
    // Val is integer
    // DestTy is float
    Val = Builder->CreateSIToFP(Val, DestTy, "inttofloat");
    break;
  case 0b11:
    // both are float
    // skip
    break;
  default:
    panic("Unknown Type, flag: " + std::to_string(flag));
  }
}

auto LLVMWrapper::load(llvm::Value *&Val, llvm::Type *Type) -> void {
  if (llvm::isa<llvm::AllocaInst>(Val)) {
    Val = Builder->CreateLoad(
        llvm::dyn_cast<llvm::AllocaInst>(Val)->getAllocatedType(), Val);
    return;
  } else if (llvm::isa<llvm::GlobalVariable>(Val)) {
    Val = Builder->CreateLoad(
        llvm::dyn_cast<llvm::GlobalVariable>(Val)->getValueType(), Val);
    return;
  } else {
    // TODO
    Val = Builder->CreateLoad(getType(DataType::Int), Val);
    return;
  }
  panic("Function cannot be loaded");
  return;
}

auto LLVMWrapper::getPtrType(llvm::Value *Val) -> llvm::Type * {
  if (llvm::isa<llvm::AllocaInst>(Val)) {
    return llvm::dyn_cast<llvm::AllocaInst>(Val)->getAllocatedType();
  } else if (llvm::isa<llvm::GlobalVariable>(Val)) {
    return llvm::dyn_cast<llvm::GlobalVariable>(Val)->getValueType();
  }

  return nullptr;
}

auto LLVMWrapper::getArrayType(llvm::Type *Type,
                               const std::vector<int> &Dimension)
    -> llvm::Type * {
  auto *AT = Type;
  for (const auto &i : Dimension | std::views::reverse) {
    AT = llvm::ArrayType::get(AT, i);
  }
  return AT;
}
} // namespace Minic