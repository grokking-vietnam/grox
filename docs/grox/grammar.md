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
expression    -> assignment 
assignment    -> IDENTIFIER "=" assignment | equality
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
statement      → exprStmt
               | forStmt
               | ifStmt
               | printStmt
               | returnStmt
               | whileStmt
               | block ;

exprStmt       → expression ";" ;
forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
                           expression? ";"
                           expression? ")" statement ;
ifStmt         → "if" "(" expression ")" statement
                 ( "else" statement )? ;
printStmt      → "print" expression ";" ;
returnStmt     → "return" expression? ";" ;
whileStmt      → "while" "(" expression ")" statement ;
block          → "{" declaration* "}" ;
```
