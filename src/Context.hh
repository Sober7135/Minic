
#pragma once

#include <llvm/IR/Value.h>

#include <map>
#include <utility>

namespace Minic {
class Scope {
private:
  Scope *Parent;
  std::map<std::string, llvm::Value *> SymbolTable;

public:
  explicit Scope(Scope *Parent = nullptr) : Parent(Parent) {}

  auto Add(const std::string &Name, llvm::Value *Val) -> void {
    SymbolTable[Name] = Val;
  }

  [[nodiscard]] auto GetParent() const -> Scope * { return Parent; }

  [[nodiscard]] auto FindCurrent(const std::string &Name) -> llvm::Value * {
    llvm::Value *Ret = nullptr;
    Ret = SymbolTable[Name];
    return Ret;
  }

  [[nodiscard]] auto Find(const std::string &Name) -> llvm::Value * {
    llvm::Value *Ret = nullptr;
    Ret = FindCurrent(Name);
    if (Parent) {
      return Parent->Find(Name);
    }

    return Ret;
  }

  [[nodiscard]] auto IsTop() const -> bool { return Parent == nullptr; }
};

} // namespace Minic
