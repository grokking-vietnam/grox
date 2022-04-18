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

object Expr:

  def show(expr: Expr): String =
    expr match
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

  private def formatNestedExpr(expr: Expr, exprShow: String): String =
    expr match
      case Literal(_) => exprShow
      case _          => s"($exprShow)"

  given exprShow: Show[Expr] = Show.show(Expr.show)

  def flatten(expr: Expr): List[Token] =
    def binary(operator: Token, l: Expr, r: Expr) = flatten(l) ::: List(operator) ::: flatten(r)

    expr match
      case Add(l, r)      => binary(grox.Operator.Plus, l, r)
      case Subtract(l, r) => binary(grox.Operator.Minus, l, r)
      case Multiply(l, r) => binary(grox.Operator.Star, l, r)
      case Divide(l, r)   => binary(grox.Operator.Slash, l, r)

      case Equal(l, r)    => binary(grox.Operator.EqualEqual, l, r)
      case NotEqual(l, r) => binary(grox.Operator.BangEqual, l, r)

      case Greater(l, r)      => binary(grox.Operator.Greater, l, r)
      case GreaterEqual(l, r) => binary(grox.Operator.GreaterEqual, l, r)
      case Less(l, r)         => binary(grox.Operator.Less, l, r)
      case LessEqual(l, r)    => binary(grox.Operator.LessEqual, l, r)

      case Negate(e) => grox.Operator.Minus :: flatten(e)
      case Not(e)    => grox.Operator.Bang :: flatten(e)

      case Grouping(e) =>
        List(grox.Operator.LeftParen) ::: flatten(e) ::: List(grox.Operator.RightParen)

      case Literal(n: Double) => List(grox.Literal.Number(n.toString))
      case Literal(s: String) => List(grox.Literal.Number(s))
      case Literal(true)      => List(grox.Keyword.True)
      case Literal(false)     => List(grox.Keyword.False)
      case Literal(null)      => List(grox.Keyword.Nil)
