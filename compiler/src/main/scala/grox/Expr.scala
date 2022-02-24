package grox

type LiteralType = Double | String | Boolean | Null

enum Expr:

  // Binary
  case Add(left: Expr, right: Expr)
  case Subtract(left: Expr, right: Expr)
  case Multiply(left: Expr, right: Expr)
  case Divide(left: Expr, right: Expr)
  case And(left: Expr, right: Expr)
  case Or(left: Expr, right: Expr)

  // Unary
  case Negate(expr: Expr)
  case Not(expr: Expr)

  case Literal(value: LiteralType)

  case Grouping(expr: Expr)

object Expr {}
