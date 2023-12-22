#pragma once

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
#include <llvm/Support/raw_ostream.h>

namespace Minic {
class LLVMWrapper {
public:
  std::unique_ptr<llvm::LLVMContext> Ctx;
  std::unique_ptr<llvm::IRBuilder<>> Builder;
  std::unique_ptr<llvm::Module> Mod;

  explicit LLVMWrapper(const std::string &ModuleID) {
    Ctx = std::make_unique<llvm::LLVMContext>();
    Mod = std::make_unique<llvm::Module>(ModuleID, *Ctx);

    // Create a new builder for the module.
    Builder = std::make_unique<llvm::IRBuilder<>>(*Ctx);
  }

  auto getType(DataType Type) -> llvm::Type *;

  /// CreateEntryBlockAlloca - Create an alloca instruction in the entry block
  /// of the function.  This is used for mutable variables etc.
  static auto CreateEntryBlockAlloca(llvm::Function *Fn, llvm::Type *Type,
                                     const std::string &Name,
                                     llvm::Value *ArraySize = nullptr)
      -> llvm::AllocaInst * {
    llvm::IRBuilder<> TmpB(&Fn->getEntryBlock(), Fn->getEntryBlock().begin());
    return TmpB.CreateAlloca(Type, ArraySize, Name.c_str());
  }

  auto convertToBool(llvm::Value *Val, std::string Name = "") -> llvm::Value *;
  auto getDefaultConstant(llvm::Type *Type) -> llvm::Constant *;

  auto implicitConvert(llvm::Value *&Val, llvm::Type *DestTy) -> void;
};
} // namespace Minic