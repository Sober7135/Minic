#include "Log.hh"

#include "Type.hh"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

namespace Minic {
using namespace llvm;

class LLVMWrapper {
public:
  std::unique_ptr<LLVMContext> Ctx;
  std::unique_ptr<IRBuilder<>> Builder;
  std::unique_ptr<Module> Mod;

  LLVMWrapper() {
    Ctx = std::make_unique<LLVMContext>();
    Mod = std::make_unique<Module>("MinicModule", *Ctx);

    // Create a new builder for the module.
    Builder = std::make_unique<IRBuilder<>>(*Ctx);
  }

  auto getType(DataType Type) -> llvm::Type * {
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

  /// CreateEntryBlockAlloca - Create an alloca instruction in the entry block
  /// of the function.  This is used for mutable variables etc.
  static auto CreateEntryBlockAlloca(llvm::Function *Fn, llvm::Type *Type,
                                     const std::string &Name,
                                     llvm::Value *ArraySize = nullptr)
      -> llvm::AllocaInst * {
    llvm::IRBuilder<> TmpB(&Fn->getEntryBlock(), Fn->getEntryBlock().begin());
    return TmpB.CreateAlloca(Type, ArraySize, Name.c_str());
  }

  auto convertToBool(Value *Val, std::string Name = "") -> Value * {

    auto Type = Val->getType();
    if (Type->isIntegerTy()) {
      auto W = Type->getIntegerBitWidth();
      return Builder->CreateICmpNE(
          Val,
          llvm::ConstantInt::get(llvm::IntegerType::get(*Ctx, W), APInt(W, 0)),
          Name);
    } else if (Type->isFloatTy()) {
      return Builder->CreateFCmpONE(
          Val, llvm::ConstantFP::get(*Ctx, APFloat(0.0)), Name);
    }
    panic("Unknown conversion to bool");
    return nullptr;
  }

  auto getDefaultConstant(llvm::Type *Type) -> llvm::Constant * {
    if (Type->isIntegerTy()) {
      auto W = Type->getIntegerBitWidth();
      return ConstantInt::get(*Ctx, APInt(W, 0));
    } else {
      return ConstantFP::get(*Ctx, APFloat(0.0));
    }
  }

  auto implicitConvert(llvm::Value *&Val, Type *DestTy) -> void {
    auto ValType = Val->getType();

    unsigned flag = 0;
    flag |= ValType->isFloatTy();
    flag |= ((unsigned)DestTy->isFloatTy() << (unsigned)1);

    // just Convert to LHS
    switch (flag) {
    case 0b00:
      // both are int
      Val = Builder->CreateIntCast(Val, DestTy, false, "inttoint");
      break;
    case 0b01:
      // Val is integer
      // DestTy is float
      Val = Builder->CreateSIToFP(Val, DestTy, "inttofloat");
      break;
    case 0b10:
      // Val is float
      // DestTy is integer
      Val = Builder->CreateFPToSI(Val, DestTy, "floattoint");
      break;
    case 0b11:
      // both are float
      // skip
      break;
    default:
      panic("Unknown Type, flag: " + std::to_string(flag));
    }
  }
};
} // namespace Minic
