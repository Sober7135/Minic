#pragma once

#include <string>
#include <variant>

// https://stackoverflow.com/questions/3796598/returning-non-ints-from-yylex
// #undef YY_DECL
// #define YY_DECL TokenType yyFlexLexer::yylex()

enum TokenType {
  kIdentifier,

  // Data types
  kLiteralInt,
  kLiteralFloat,
  // TODO 'char' is optional

  // Keywords
  kInt,
  kFloat,
  kFor,
  kIf,
  kElse,
  // TODO 'elseif' is optional
  kWhile,
  kReturn,

  // Operators
  kAssign,
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
  // TODO '|' (BitOr) is optional

  // Punctuation
  kComma,
  kSemicolon,
  kLeftParen,
  kRightParen,
  kLeftBrace,
  kRightBrace,

  // End of File
  kEndOfFile,
};

using YYLVAL = std::variant<int, float, std::string>;
inline YYLVAL yylval;