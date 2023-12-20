#pragma once
#include <cstdlib>
#include <iostream>
#include <string>

inline void panic(const std::string &Msg) {
  std::cerr << Msg.c_str() << std::endl;
  exit(-1);
}

inline void unimplement() { panic("unimplement!"); }
