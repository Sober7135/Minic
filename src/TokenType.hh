#pragma once

enum TokenType {
  Identifier,

  // Data types
  LiteralInt,
  LiteralFloat,
  LiteralChar,
  // TODO 'char' is optional

  // Keywords
  Int,
  Float,
  Char,
  Void,
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
  // TODO '%'

  // Punctuation
  Period,
  Comma,
  Semicolon,
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,
  LeftSquareBrace,
  RightSquareBrace,
  SingleQuote,
  DoubleQuote,

  // End of File
  EndOfFile,

  UnaryMinus,
};
