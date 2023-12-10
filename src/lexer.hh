#pragma once

#include "token2string.hh"
#include "tokenType.hh"

#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>

struct Token {
  TokenType Type;
  std::optional<std::variant<int, float, std::string>> Val;

  Token() : Type(), Val() {}
  explicit Token(
      TokenType Type,
      std::optional<std::variant<int, float, std::string>> Val = std::nullopt)
      : Type(Type), Val(std::move(Val)) {}
};

inline Token yylval{};

inline auto Token2String(const Token &T) -> std::string {
  auto Val = [](const Token &T) {
    if (T.Type == TokenType::LiteralInt) {
      return std::format("{}", std::get<0>(T.Val.value()));
    } else if (T.Type == TokenType::LiteralFloat) {
      return std::format("{}", std::get<float>(T.Val.value()));
    } else if (T.Type == TokenType::Identifier) {
      return std::get<std::string>(T.Val.value());
    } else {
      return std::string{"None"};
    }
  }(T);

  return std::format("({}, {})", TokenType2String(T.Type), Val);
}
