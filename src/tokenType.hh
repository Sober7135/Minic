#pragma once

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
  // TODO '%'

  // Punctuation
  Comma,
  Semicolon,
  LeftParen,
  RightParen,
  LeftBrace,
  RightBrace,

  // End of File
  EndOfFile,

  UnaryMinus,
};
