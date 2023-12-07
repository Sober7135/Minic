%{
    #include "lexer.hh"  
%}

%option noyywrap c++
digit               [0-9]
letter              [a-zA-Z_]
float               ({digit}+\.{digit}*)|(\.{digit}+)
int                 {digit}+
identifier          {letter}+({digit}|{letter})*
newline             \n
whitespace          [ \t]+

%%
{whitespace}        {} // skip
{newline}           {}
{int}               { yylval = std::stoi(yytext); return TokenType::kInt; }
{float}             { yylval = std::stof(yytext); return TokenType::kFloat; }// TODO char option 

"for"               { return TokenType::kFor; }
"if"                { return TokenType::kIf; }
"else"              { return TokenType::kElse; } // TODO "Elseif" { return TokenType::kElse; }
"while"             { return TokenType::kWhile; }
"return"            { return TokenType::kReturn; }
"+"                 { return TokenType::kPlus; }
"-"                 { return TokenType::kMinus; }
"*"                 { return TokenType::kMultiply; }
"/"                 { return TokenType::kDivide; }
"<"                 { return TokenType::kLess; }
"<="                { return TokenType::kLessEqual; }
"=="                { return TokenType::kEqual; }
"!="                { return TokenType::kNotEqual; }
">"                 { return TokenType::kGreater; }
">="                { return TokenType::kGreaterEqual; }
"&&"                { return TokenType::kAnd; }
"||"                { return TokenType::kOr; } 

","                 { return TokenType::kComma; }
";"                 { return TokenType::kSemicolon; }
"("                 { return TokenType::kLeftParen; }
")"                 { return TokenType::kRightParen; }
"{"                 { return TokenType::kLeftBrace; }
"}"                 { return TokenType::kRightBrace; }

<<EOF>>             { return TokenType::kEndOfFile; }

{identifier}        { yylval = yytext; return TokenType::kIdentifier; }
%%