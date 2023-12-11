%{
    #include "type.hh"  
    #include "lexer.hh"
%}

%option noyywrap c++
%option outfile="flex.cc"
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
{int}               { yylval = Token(TokenType::LiteralInt, std::make_optional<std::variant<int, float, std::string>>(std::stoi(yytext))); return TokenType::LiteralInt; }
{float}             { yylval = Token(TokenType::LiteralFloat, std::make_optional<std::variant<int, float, std::string>>(std::stof(yytext))); return TokenType::LiteralFloat; }// TODO char option 

"int"               { yylval = Token(TokenType::Int); return TokenType::Int; }
"float"             { yylval = Token(TokenType::Float); return TokenType::Float; }
"void"              { yylval = Token(TokenType::Void); return TokenType::Void; }
"for"               { yylval = Token(TokenType::For); return TokenType::For; }
"if"                { yylval = Token(TokenType::If); return TokenType::If; }
"else"              { yylval = Token(TokenType::Else); return TokenType::Else; } // TODO "Elseif" { return TokenType::kElse; }
"while"             { yylval = Token(TokenType::While); return TokenType::While; }
"return"            { yylval = Token(TokenType::Return); return TokenType::Return; }
"="                 { yylval = Token(TokenType::Assign); return TokenType::Assign; }
"+"                 { yylval = Token(TokenType::Plus); return TokenType::Plus; }
"-"                 { yylval = Token(TokenType::Minus); return TokenType::Minus; }
"*"                 { yylval = Token(TokenType::Multiply); return TokenType::Multiply; }
"/"                 { yylval = Token(TokenType::Divide); return TokenType::Divide; }
"<"                 { yylval = Token(TokenType::Less); return TokenType::Less; }
"<="                { yylval = Token(TokenType::LessEqual); return TokenType::LessEqual; }
"=="                { yylval = Token(TokenType::Equal); return TokenType::Equal; }
"!="                { yylval = Token(TokenType::NotEqual); return TokenType::NotEqual; }
">"                 { yylval = Token(TokenType::Greater); return TokenType::Greater; }
">="                { yylval = Token(TokenType::GreaterEqual); return TokenType::GreaterEqual; }
"&&"                { yylval = Token(TokenType::And); return TokenType::And; }
"||"                { yylval = Token(TokenType::Or); return TokenType::Or; } 

","                 { yylval = Token(TokenType::Comma); return TokenType::Comma; }
";"                 { yylval = Token(TokenType::Semicolon); return TokenType::Semicolon; }
"("                 { yylval = Token(TokenType::LeftParen); return TokenType::LeftParen; }
")"                 { yylval = Token(TokenType::RightParen); return TokenType::RightParen; }
"{"                 { yylval = Token(TokenType::LeftBrace); return TokenType::LeftBrace; }
"}"                 { yylval = Token(TokenType::RightBrace); return TokenType::RightBrace; }

<<EOF>>             { yylval = Token(TokenType::EndOfFile); return TokenType::EndOfFile; }

{identifier}        { yylval = Token(TokenType::Identifier, std::make_optional<std::variant<int, float, std::string>>(yytext)); return TokenType::Identifier; }
%%