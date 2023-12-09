%{
    #include "type.hh"  
    #include "lexer.hh"
%}

%option noyywrap c++
%option outfile="lexer.cc"
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
{int}               { yylval = std::stoi(yytext); return TokenType::LiteralInt; }
{float}             { yylval = std::stof(yytext); return TokenType::LiteralFloat; }// TODO char option 

"int"               { return TokenType::Int; }
"float"             { return TokenType::Float; }
"for"               { return TokenType::For; }
"if"                { return TokenType::If; }
"else"              { return TokenType::Else; } // TODO "Elseif" { return TokenType::kElse; }
"while"             { return TokenType::While; }
"return"            { return TokenType::Return; }
"="                 { return TokenType::Assign; }
"+"                 { return TokenType::Plus; }
"-"                 { return TokenType::Minus; }
"*"                 { return TokenType::Multiply; }
"/"                 { return TokenType::Divide; }
"<"                 { return TokenType::Less; }
"<="                { return TokenType::LessEqual; }
"=="                { return TokenType::Equal; }
"!="                { return TokenType::NotEqual; }
">"                 { return TokenType::Greater; }
">="                { return TokenType::GreaterEqual; }
"&&"                { return TokenType::And; }
"||"                { return TokenType::Or; } 

","                 { return TokenType::Comma; }
";"                 { return TokenType::Semicolon; }
"("                 { return TokenType::LeftParen; }
")"                 { return TokenType::RightParen; }
"{"                 { return TokenType::LeftBrace; }
"}"                 { return TokenType::RightBrace; }

<<EOF>>             { return TokenType::EndOfFile; }

{identifier}        { yylval = yytext; return TokenType::Identifier; }
%%