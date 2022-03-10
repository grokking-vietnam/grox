# Grox Grammar

## Expression

```
expression     → literal
               | unary
               | binary
               | grouping ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
grouping       → "(" expression ")" ;
unary          → ( "-" | "!" ) expression ;
binary         → expression operator expression ;
operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
               | "+"  | "-"  | "*" | "/" ;
```

## Expression (ordered grammar, for parser)
```
expression    -> equality
equality      -> comparison (("!=" | "==") comparison)*
comparison    -> factor (("<" | "<=" | ">" | ">=") factor)*
factor        -> term (("+" | "-") term)* 
term          -> unary (("*" | "/") unary)*
unary         -> ("-" | "!") unary 
              | primary
primary       -> NUMBER | STRING | "true" | "false" | "nil" 
              | groupping
groupping     -> "(" expression ")"
```
