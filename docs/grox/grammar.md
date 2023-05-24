# Grox Grammar

## Expression

```
expression     -> literal
               | unary
               | binary
               | grouping ;

literal        -> NUMBER | STRING | "true" | "false" | "nil" ;
logical
grouping       -> "(" expression ")" ;
unary          -> ( "-" | "!" ) expression ;
binary         -> expression operator expression ;
operator       -> "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
```

## Expression (ordered grammar, for parser)
```
expression    -> logic_or ;
logic_or      -> logic_and ( "or" logic_and )* ;
logic_and     -> equality ( "and" equality )* ;
equality      -> comparison (("!=" | "==") comparison)*
comparison    -> factor (("<" | "<=" | ">" | ">=") factor)*
factor        -> term (("+" | "-") term)*
term          -> unary (("*" | "/") unary)*
unary         -> ("-" | "!") unary
              | primary
primary       -> "true" | "false" | "nil"
              | NUMBER | STRING
              | "(" expression ")"
              | IDENTIFIER ;
grouping     -> "(" expression ")"
```

## Statements
```
program        → declaration* EOF ;

declaration    → varDecl
               | statement ;

statement      -> exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | whileStmt
               | assignStmt
               | block ;

assignmentExpr -> IDENTIFIER "=" expression
assignmentStmt -> assignmentExpr ";"

exprStmt       -> expression ";" ;
forStmt        -> "for" "(" ( varDecl | exprStmt | ";" )
                           expression? ";"
                           assignmentExpr? ")" statement ;
ifStmt         -> "if" "(" expression ")" statement
                 ( "else" statement )? ;
printStmt      -> "print" expression ";" ;
whileStmt      -> "while" "(" expression ")" statement ;

block          -> "{" statement* "}" ;
varDecl        -> "var" IDENTIFIER ( "=" expression )? ";" ;

```
