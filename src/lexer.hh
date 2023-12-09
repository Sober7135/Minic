#pragma once

#include <string>
#include <variant>

// https://stackoverflow.com/questions/3796598/returning-non-ints-from-yylex
// #undef YY_DECL
// #define YY_DECL TokenType yyFlexLexer::yylex()

using YYLVAL = std::variant<int, float, std::string>;
inline YYLVAL yylval;

enum TokenType {
  Identifier,

  // Data types
  LiteralInt,
  LiteralFloat,
  // TODO 'char' is optional

  // Keywords
  Int,
  Float,
  For,
  If,
  Else,
  // TODO 'elseif' is optional
  While,
  Return,

  // Operators
  Assign,
  Plus,
  Minus,
  Multiply,
  Divide,
  Less,
  LessEqual,
  Equal,
  NotEqual,
  Greater,
  GreaterEqual,
  And,
  Or,
  // TODO '|' (BitOr) is optional

  // Punctuation
  Comma,
  Semicolon,
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,

  // End of File
  EndOfFile,
};
