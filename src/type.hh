#pragma once

#include "tokenType.hh"
#include <stdexcept>

enum class DataType {
  kInt,
  kFloat,
  kVoid,
};

inline auto TokenType2DataType(TokenType token) -> DataType {
  switch (token) {
  case TokenType::Int:
    return DataType::kInt;
  case TokenType::Float:
    return DataType::kFloat;
  case TokenType::Void:
    return DataType::kVoid;
  default:
    throw std::invalid_argument("invalid token");
  }
}