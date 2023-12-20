#include "AST.hh"
#include <string>

namespace Minic {

Declarator::operator std::string() {
  std::string Ret = Name;
  for (const auto &D : Dimension) {
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
  for (const auto &Child : Children) {
    Ret += "\n  " + std::string(*Child);
  }
  return Ret;
}

FunctionDecl::operator std::string() {
  auto IsPrototype_ = isPrototype();
  if (IsPrototype_) {
    return "FunctionPrototype " + Name;
  }
  return "FunctionDecl " + Name;
}
} // namespace Minic