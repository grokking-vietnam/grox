package grox

type LiteralType = Double | String | Boolean | Null

enum Expr:

  // Binary

  // arithmetic
  case Add(left: Expr, right: Expr)
  case Subtract(left: Expr, right: Expr)
  case Multiply(left: Expr, right: Expr)
  case Divide(left: Expr, right: Expr)

  // comparison
  case Greater(left: Expr, right: Expr)
  case GreaterEqual(left: Expr, right: Expr)
  case Less(left: Expr, right: Expr)
  case LessEqual(left: Expr, right: Expr)
  case Equal(left: Expr, right: Expr)
  case NotEqual(left: Expr, right: Expr)

  // Unary
  case Negate(expr: Expr)
  case Not(expr: Expr)

  case Literal(value: LiteralType)

  case Grouping(expr: Expr)

object Expr {}
