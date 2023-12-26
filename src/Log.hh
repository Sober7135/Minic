#pragma once

#include <cstdlib>
#include <llvm/Support/raw_ostream.h>
#include <string>

inline void panic(const std::string &Msg) {
  llvm::errs() << Msg.c_str() << '\n';
  exit(-1);
}

inline void unimplement(const std::string &Str = "") {
  panic(Str + " " + "unimplement!");
}

inline void warning(const std::string &Str) {
  llvm::errs() << "Warning: " << Str << '\n';
}