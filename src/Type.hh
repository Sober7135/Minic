#pragma once

#include <map>
#include <string>
namespace Minic {
enum class DataType {
  Int,
  Float,
  Char,
  Void,
};

enum class BinaryOperator {
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
  Precent,

  LogicalOr,
  LogicalAnd,

  BitwiseAnd,
  BitwiseOr,
  BitwiseXor,
};

enum class UnaryOperator {
  Plus,
  Minus,
  BitwiseNot,
  LogicalNot,
};

inline std::map<std::string, DataType> String2DataType{
    {"int", DataType::Int},
    {"float", DataType::Float},
    {"char", DataType::Char},
    {"void", DataType::Void},
};

inline std::map<DataType, std::string> DataType2String{
    {DataType::Int, "int"},
    {DataType::Float, "float"},
    {DataType::Char, "char"},
    {DataType::Void, "void"},
};

inline std::map<std::string, BinaryOperator> String2BinaryOperator{
    {"=", BinaryOperator::Assign},     {"<", BinaryOperator::Less},
    {"<=", BinaryOperator::LessEqual}, {"==", BinaryOperator::Equal},
    {">", BinaryOperator::Greater},    {">=", BinaryOperator::GreaterEqual},
    {"!=", BinaryOperator::NotEqual},  {"+", BinaryOperator::Plus},
    {"-", BinaryOperator::Minus},      {"*", BinaryOperator::Multiply},
    {"%", BinaryOperator::Precent},    {"/", BinaryOperator::Divide},
    {"||", BinaryOperator::LogicalOr}, {"&&", BinaryOperator::LogicalAnd},
    {"&", BinaryOperator::BitwiseAnd}, {"|", BinaryOperator::BitwiseOr},
    {"^", BinaryOperator::BitwiseXor},
};

inline std::map<std::string, UnaryOperator> String2UnaryOperator{
    {"+", UnaryOperator::Plus},
    {"-", UnaryOperator::Minus},
    {"!", UnaryOperator::LogicalNot},
    {"~", UnaryOperator::BitwiseNot},
};

} // namespace Minic
