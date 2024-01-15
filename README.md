
## What is supported?

~~still have many bugs and many implementations are ungraceful~~
- implicit conversion
- variable shadowing
- multiple dimensions array with initializer
- ...
`struct`, pointer are not supported...
You can check the `test` dir to see what has been supported...

## Dependency
- I use `std::format`, so `c++2a`
- `LLVM` 17.0.6
- `git`


## How to compile
1. install dependencies above, including LLVM(17.0.6 is better), clang(I use clang to link the *.o file to dynamic library and also use it to test my code), cmake, ninja(optional), git(git clone the antlr cpp runtime), python3

2. modify the CMakeLists.txt, I hard code the configuration, so maybe you should change it to make it fit on your machine.

3. compile

## Grammar
```Antlr4
grammar Minic;

// * >= 0 
// + >= 1 
// ? 0 | 1

program
    : declaration*
	  ;

declaration
    : globalVarDeclStmt 
    | functionDecl
	  ;

globalVarDeclStmt
    : varDecl Semicolon
    ;

varDeclStmt 
    : varDecl Semicolon
    ;

varDecl
    : dataType initDeclaratorList
	  ;

dataType
    : Int 
    | Float 
    | Char
    | Void
	  ;

initDeclaratorList
    : initDeclarator (Comma initDeclarator)*
	  ;

initDeclarator
    : declarator (Assign initializer)?
	  ;

declarator
    : Identifier (LeftSquareBrace LiteralInt RightSquareBrace)* // declaring array must speicify size...  
	  ;

// int a[123]; array size must be explicit and only can be constant.

initializer
    : expr
    | LeftBrace initializer (Comma initializer)* RightBrace // array initializer
	  ;

literal
    : LiteralInt 
    | LiteralFloat 
    | LiteralChar
	  ;
    
functionDecl
    : dataType Identifier LeftParen parmVarList? RightParen compoundStmt  // function definition
    | dataType Identifier LeftParen parmVarList? RightParen Semicolon     // function declaration
    ;

parmVarList
    : parmVarDecl (Comma parmVarDecl)*
    ;

parmVarDecl
    : dataType declarator
    ;
    
compoundStmt
    : LeftBrace statement* RightBrace
    ;

statement
    : ifStmt
    | whileStmt
    | exprStmt
    | varDeclStmt
    | returnStmt
    | breakStmt
    | continueStmt
    | compoundStmt
    ;

ifStmt
    : If LeftParen expr RightParen statement (Else statement)?
    ;
    
whileStmt
    : While LeftParen expr RightParen statement
    ;

exprStmt
    : expr Semicolon
    ;

returnStmt
    : Return expr? Semicolon
    ;
    
breakStmt
    : Break Semicolon
    ;
    
continueStmt
    : Continue Semicolon
    ;
    
expr
    : Identifier LeftParen varList? RightParen                          # CallExpr
    | Identifier                                                        # IdentifierExpr
    | literal                                                           # LiteralExpr
    | Base=expr LeftSquareBrace Selector=expr RightSquareBrace          # ArraySubscriptExpr
    | Op=(Plus | Minus | LogicalNot | BitwiseNot) expr                  # UnaryExpr
    | LHS=expr Op=(Asterisk | Percent | Divide) RHS=expr                # MulModDivExpr
    | LHS=expr Op=(Plus | Minus) RHS=expr                               # AddSubExpr
    // | LHS=expr Op=() bitwise left shift and so on
    | LHS=expr Op=(Greater | GreaterEqual | Less | LessEqual) RHS=expr  # RelationalLGExpr
    | LHS=expr Op=(Equal | NotEqual) RHS=expr                           # RelationalENEExpr
    | LHS=expr Op=BitwiseAnd RHS=expr                                   # BitwiseAndExpr
    | LHS=expr Op=BitwiseXor RHS=expr                                   # BitwiseXorExpr
    | LHS=expr Op=BitwiseOr RHS=expr                                    # BitwiseOrExpr
    | LHS=expr Op=LogicalAnd RHS=expr                                   # LogicalAndExpr
    | LHS=expr Op=LogicalOr RHS=expr                                    # LogicalOrExpr
    | LHS=expr Op=Assign RHS=expr                                       # AssignExpr
    | '(' expr ')'                                                      # ParenExpr
    ;

varList
    : expr (Comma expr)*
    ;
    
// Token
Int
    : 'int' 
	  ;
Float
    : 'float' 
	  ;
Char
    : 'char' 
	  ;
Void
    : 'void' 
    ;
LiteralInt
    : DigitSequence
	  ;
LiteralFloat
    : (DigitSequence? Period DigitSequence)
	  | (DigitSequence Period)
	  ;
LiteralChar
    : SingleQuote Ascii SingleQuote
	  ;
If
    : 'if' 
	  ;
Else
    : 'else' 
	  ;
ElseIf
    : 'elseif' 
	  ;
For
    : 'for' 
	  ;
While
    : 'while' 
	  ;
Return
    : 'return' 
    ;
Break
    : 'break' 
	  ;
Continue
    : 'continue' 
	  ;
Assign
    : '=' 
    ;
Plus 
    : '+' 
    ;
Minus
    : '-' 
    ;
Asterisk
    : '*' 
    ;
Divide
    : '/' 
    ;
Less
    : '<' 
    ;
LessEqual
    : '<=' 
    ;
Equal
    : '==' 
    ;
NotEqual
    : '!=' 
    ;
Greater
    : '>' 
    ;
GreaterEqual
    : '>=' 
    ;
LogicalAnd 
    : '&&' 
    ;
BitwiseAnd
    : '&'
    ;
LogicalOr 
    : '||' 
    ;
BitwiseOr
    : '|'
    ;
LogicalNot
    : '!'
    ;
BitwiseNot
    : '~'
    ;
BitwiseXor
    : '^'
    ;
Period
    : '.' 
    ;
Comma
    : ',' 
    ;
Semicolon
    : ';' 
    ;
LeftParen
    : '(' 
    ;
RightParen
    : ')' 
    ;
LeftBrace
    : '{' 
    ;
RightBrace
    : '}' 
    ;
LeftSquareBrace
    : '[' 
    ;
RightSquareBrace
    : ']' 
    ;
SingleQuote
    : '\'' 
    ;
DoubleQuote
    : '"' 
    ;
Percent
    : '%'
    ;
Identifier
    : NonDigit+ (NonDigit | Digit)*
	  ;
    
NewLine
    : [\r\n] -> skip
    ;

WhiteSpace
    : [ \t] -> skip
    ;

fragment NonDigit
    : [a-zA-Z_]
	  ;
fragment Digit
    : [0-9]
	  ;
fragment DigitSequence
    : Digit+
	  ;
fragment Ascii
    : [\u0000-\u007F]
	  ;


BlockComment
    : '/*' .*? '*/' -> skip
    ;

LineComment
    : '//' ~[\r\n]* -> skip
    ;
```

