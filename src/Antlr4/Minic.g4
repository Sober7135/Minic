grammar Minic;

// * >= 0 
// + >= 1 
// ? 0 | 1

program
    : declaration*
	  ;

declaration
    : globalVarDeclStmt  // not support function declaration, something like  `int test(int);`
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
    : dataType Identifier LeftParen parmVarList? RightParen compoundStmt? // not function declaration
    | dataType Identifier LeftParen parmVarList? RightParen Semicolon     // not function declaration
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
    // | forStmt
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

// removing forStmt supporting, because if we support for, we must deal with left value and assignment expression
// forStmt
//     : For LeftParen expr Semicolon expr Semicolon expr RightParen statement
//     ;

exprStmt
    : expr Semicolon
    ;

returnStmt
    : Return expr Semicolon
    ;
    
breakStmt
    : Break Semicolon
    ;
    
continueStmt
    : Continue Semicolon
    ;

expr
    : assignment
    ;

assignment
    : equality (Assign equality)*
    ;

equality
    : comparison ((NotEqual | Equal) comparison)*
    ;
    
comparison
    : term ((Less| LessEqual | Greater | GreaterEqual) term) *
    ;

term
    : factor ((Plus | Minus) factor) *
    ;

factor
    : unary (( Multiply | Divide) unary)*
    ;

unary
    : (Plus | Minus) unary
    | primary
    ;

primary
    : identifierExpr
    | literal
    | parenExpr
    ;

identifierExpr
    : Identifier
    | callExpr
    ;

callExpr
    : Identifier LeftParen varList? RightParen
    ;

varList
    : expr (Comma expr)*
    ;
    
parenExpr
    : LeftBrace expr RightBrace
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
Multiply
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
And 
    : '&&' 
    ;
Or 
    : '||' 
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