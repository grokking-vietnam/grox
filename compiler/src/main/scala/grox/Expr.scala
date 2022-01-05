package grox

enum Expr:

  case Binary(left: Expr, operator: MyToken[MyOperator], right: Expr)
  case Grouping(expression: Expr)
  case Unary(operator: MyToken[MyUnary], expression: Expr)
  case Literal(value: MyToken[MyLiteral])
