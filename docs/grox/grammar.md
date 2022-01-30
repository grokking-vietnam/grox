# Grox Grammar

## Expression

```
expression        → literal
                  | operator
                  | grouping ;

literal           → Double | Boolean ;
grouping          → "(" expression ")" ;
binary            → expression operator expression ;
operator          → "==" | "!=" | "<" | "<=" | ">" | ">="
                  | "+"  | "-"  | "*" | "/"
                  | "&&" | "||" | "!" ;
```
