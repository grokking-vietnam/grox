package grox

import cats.Show

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

object Expr {

  def show(expr: Expr): String =
    expr match {
      case Add(left, right) =>
        s"${formatNestedExpr(left, show(left))} + ${formatNestedExpr(right, show(right))}"
      case Subtract(left, right) =>
        s"${formatNestedExpr(left, show(left))} - ${formatNestedExpr(right, show(right))}"
      case Multiply(left, right) =>
        s"${formatNestedExpr(left, show(left))} × ${formatNestedExpr(right, show(right))}"
      case Divide(left, right) =>
        s"${formatNestedExpr(left, show(left))} ÷ ${formatNestedExpr(right, show(right))}"

      case Negate(expr) => s"-${formatNestedExpr(expr, show(expr))}"
      case Not(expr)    => s"!${formatNestedExpr(expr, show(expr))}"

      case Greater(left, right) =>
        s"${formatNestedExpr(left, show(left))} > ${formatNestedExpr(right, show(right))}"
      case GreaterEqual(left, right) =>
        s"${formatNestedExpr(left, show(left))} ≥ ${formatNestedExpr(right, show(right))}"
      case Less(left, right) =>
        s"${formatNestedExpr(left, show(left))} < ${formatNestedExpr(right, show(right))}"
      case LessEqual(left, right) =>
        s"${formatNestedExpr(left, show(left))} ≤ ${formatNestedExpr(right, show(right))}"
      case Equal(left, right) =>
        s"${formatNestedExpr(left, show(left))} == ${formatNestedExpr(right, show(right))}"
      case NotEqual(left, right) =>
        s"${formatNestedExpr(left, show(left))} ≠ ${formatNestedExpr(right, show(right))}"

      case Grouping(expr) => s"${formatNestedExpr(expr, show(expr))})"

      case Literal(value) => value.toString
    }

  private def formatNestedExpr(expr: Expr, exprShow: String): String =
    expr match {
      case Literal(_) => exprShow
      case _          => s"($exprShow)"
    }

  given exprShow: Show[Expr] = Show.show(Expr.show)
}
