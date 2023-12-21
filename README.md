# mcc

compiler for mini-c 😅

## Grammar

```
G[program]:
  program -> ExtDefList

  ExtDefList -> ExtDef  ExtDefList | ε

  ExtDef -> Specifier  ExtDecList ; | Specifier  FuncDec  CompSt

  Specifier -> int | float

  ExtDecList -> VarDec | VarDec , ExtDecList

  VarDec -> ID

  FuncDec -> ID ( VarList )  | ID ( )

  VarList -> ParamDec , VarList   |   ParamDec

  ParamDec -> Specifier VarDec

  CompSt -> { DefList  StmList }

  StmList -> Stmt  StmList | ε

  Stmt -> Expr ;  |  CompSt  | return Expr ; | if ( Expr ) Stmt   | if ( Expr ) Stmt else Stmt   | while ( Expr ) Stmt

  DefList -> Def DefList | ε

  Def -> Specifier DecList ;

  DecList -> Dec | Dec , DecList

  Dec -> VarDec  |  VarDec = Expr

  Expr -> Expr = Expr  | Expr && Expr |  Expr || Expr | Expr < Expr | Expr <= Expr
	 	| Expr == Expr | Expr != Expr	| Expr > Expr | Expr >= Expr
		| Expr + Expr	| Expr - Expr  | Expr * Expr	| Expr / Expr	| ID | INT | FLOAT
		| ( Expr )		| - Expr  |  ! Expr  | ID ( Args )  | ID ( )

  Args -> Expr , Args  | Expr
```

## Token

`int`, `float`, `char`(optional)

`for`, `if`, `else`, `elseif`(optional), `while`, `return`, `main` (????)

`(`, `)`, `&&`, `||`, `|`(optional), `<`, `<=`, `==`, `!=`, `>`, `>=`,
`+`, `-`(Expr - Expr), `*`, `/`, `-`(-Expr)

`,`, `;`,  `{`, `}`

## Detail

### float
not support e/E

1. Global Variable auto initialize to 0
2. conversion, assign 