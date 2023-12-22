#include "Wrapper.hh"
#include "Log.hh"

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

auto LLVMWrapper::convertToBool(Value *Val, std::string Name) -> Value * {

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

auto LLVMWrapper::getDefaultConstant(llvm::Type *Type) -> llvm::Constant * {
  if (Type->isIntegerTy()) {
    auto W = Type->getIntegerBitWidth();
    return ConstantInt::get(*Ctx, APInt(W, 0));
  } else {
    return ConstantFP::get(*Ctx, APFloat(0.0));
  }
}

auto LLVMWrapper::implicitConvert(llvm::Value *&Val, Type *DestTy) -> void {
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
    std::cout << Val->getType()->getTypeID() << '\n'
              << DestTy->getTypeID() << "\n\n\n\n"
              << std::endl;

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
} // namespace Minic