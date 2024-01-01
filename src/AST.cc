#include "AST.hh"

#include <string>

#include "Type.hh"

namespace Minic {

Declarator::operator std::string() {
  std::string Ret = Name;
  for (const auto& D : Dimension) {
    Ret += "[" + std::to_string(D) + "]";
  }
  return Ret;
}

Initializer::operator std::string() {
  if (IsLeaf) {
    // Is Leaf
    return "  " + std::string(*TheExpr);
  }
  std::string Ret = "ArrayInitialzer";
  for (const auto& Child : Children) {
    Ret += "\n  " + std::string(*Child);
  }
  return Ret;
}

FunctionDecl::operator std::string() {
  auto IsPrototype_ = isPrototype();
  std::string Ret = "";
  if (IsPrototype_) {
    Ret = "FunctionPrototype\n";
  } else {
    Ret = "FunctionDecl\n";
  }
  Ret += "  ReturnType: " + DataType2String[Type] + "\n";
  Ret += "  Name: " + Name + "\n";
  Ret += "  VarList: ";
  if (VarList.empty()) {
    Ret += "void\n";
  } else {
    for (const auto& i : VarList) {
      Ret += DataType2String[i->getType()] + " ";
    }
    Ret += '\n';
  }
  return Ret;
}

ArraySubscriptExpr::operator std::string() {
  std::string Ret = "ArraySubscriptExpr";
  return Ret;
}
}  // namespace Minic