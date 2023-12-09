#pragma once

#include "lexer.hh"
#include <stdexcept>

enum class DataType {
  kInt,
  kFloat,
};

inline auto TokenType2DataType(TokenType token) -> DataType {
  switch (token) {
  case TokenType::Int:
    return DataType::kInt;
  case TokenType::Float:
    return DataType::kFloat;
  default:
    throw std::invalid_argument("invalid token");
  }
}