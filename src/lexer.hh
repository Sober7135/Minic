#pragma once

#include <iostream>
#include <string>
#include <variant>

// https://stackoverflow.com/questions/3796598/returning-non-ints-from-yylex
// #undef YY_DECL
// #define YY_DECL TokenType yyFlexLexer::yylex()

enum TokenType {
  kIdentifier,

  // Data types
  kInt,
  kFloat,
  // kChar, // 'char' is optional

  // Keywords
  kFor,
  kIf,
  kElse,
  // kElseIf, // 'elseif' is optional
  kWhile,
  kReturn,

  // Operators
  kPlus,
  kMinus, // for both Minus and unary minus
  kMultiply,
  kDivide,
  kLess,
  kLessEqual,
  kEqual,
  kNotEqual,
  kGreater,
  kGreaterEqual,
  kAnd,
  kOr,
  // kBitOr, // 'BitOr' is optional

  // Punctuation
  kComma,
  kSemicolon,
  kLeftParen,
  kRightParen,
  kLeftBrace,
  kRightBrace,

  // End of File
  kEndOfFile
};

using YYLVAL = std::variant<int, float, std::string>;
inline YYLVAL yylval;