#pragma once

#include "TokenType.hh"
#include <map>
#include <stdexcept>
#include <string>
namespace Minic {
enum class DataType {
  Int,
  Float,
  Char,
  Void,
};

enum class BinayOperator {
  Assign,

  // Comparason
  Less,
  LessEqual,
  Greater,
  GreaterEqual,
  Equal,
  NotEqual,

  Plus,
  Minus,
  Multiply,
  Divide,

  Or,
  And,
};

enum class UnaryOperator {
  Plus,
  Minus,
};

inline auto TokenType2DataType(TokenType token) -> DataType {
  switch (token) {
  case TokenType::Int:
    return DataType::Int;
  case TokenType::Float:
    return DataType::Float;
  case TokenType::Void:
    return DataType::Void;
  default:
    throw std::invalid_argument("invalid token");
  }
}

inline std::map<std::string, DataType> String2DataType{
    {"int", DataType::Int},
    {"float", DataType::Float},
    {"char", DataType::Char},
};

inline std::map<DataType, std::string> DataType2String{
    {DataType::Int, "int"},
    {DataType::Float, "float"},
    {DataType::Char, "char"},
};

inline std::map<std::string, BinayOperator> String2BinaryOperator{
    {"=", BinayOperator::Assign},     {"<", BinayOperator::Less},
    {"<=", BinayOperator::LessEqual}, {"==", BinayOperator::Equal},
    {">", BinayOperator::Greater},    {">=", BinayOperator::GreaterEqual},
    {"!=", BinayOperator::NotEqual},  {"+", BinayOperator::Plus},
    {"-", BinayOperator::Minus},      {"*", BinayOperator::Multiply},
    {"/", BinayOperator::Divide},     {"||", BinayOperator::Or},
    {"&&", BinayOperator::And},

};

inline std::map<std::string, UnaryOperator> String2UnaryOperator{
    {"+", UnaryOperator::Plus},
    {"-", UnaryOperator::Minus},
};

} // namespace Minic
