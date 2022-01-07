package grox

import cats.kernel.Comparison.GreaterThan

enum Expr:
  case Add(left: Expr, right: Expr)
  case Subtract(left: Expr, right: Expr)
  case Multiply(left: Expr, right: Expr)
  case Divide(left: Expr, right: Expr)
  case Negate(expr: Expr)
  case Minus(expr: Expr)
  case Number(value: Int | Double)
  case Str(value: String)
  case Grouping(expr: Expr)
