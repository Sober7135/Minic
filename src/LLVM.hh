#include "Log.hh"

#include "Type.hh"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
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

  auto GetType(DataType Type) -> llvm::Type * {
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

  auto ConvertToBool(Value *Val, std::string Name = "") -> Value * {

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
};
} // namespace Minic
