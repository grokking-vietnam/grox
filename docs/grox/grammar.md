# Grox Grammar

## Expression

```
expression        → literal
                  | unary
                  | binary
                  | grouping ;

literal           → Double | Boolean ;
grouping          → "(" expression ")" ;
binary            → expression operator expression ;
unary             → ( "-" | "!" ) operator ;
operator          → "==" | "!=" | "<" | "<=" | ">" | ">="
                  | "+"  | "-"  | "*" | "/"
                  | "&&" | "||" | "!" ;
```
