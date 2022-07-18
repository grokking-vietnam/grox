package grox

import cats.Show

// We use Unit to represent the absence of a value or nil in the language.
type LiteralType = Double | String | Boolean | Unit

enum Expr:

  // Binary

  // arithmetic
  case Add(tag: Span, left: Expr, right: Expr)
  case Subtract(tag: Span, left: Expr, right: Expr)
  case Multiply(tag: Span, left: Expr, right: Expr)
  case Divide(tag: Span, left: Expr, right: Expr)

  // comparison
  case Greater(tag: Span, left: Expr, right: Expr)
  case GreaterEqual(tag: Span, left: Expr, right: Expr)
  case Less(tag: Span, left: Expr, right: Expr)
  case LessEqual(tag: Span, left: Expr, right: Expr)
  case Equal(tag: Span, left: Expr, right: Expr)
  case NotEqual(tag: Span, left: Expr, right: Expr)

  // assignment

  case Assign(tag: Span, name: String, value: Expr)

  // logic
  case Or(tag: Span, left: Expr, right: Expr)
  case And(tag: Span, left: Expr, right: Expr)

  // Unary
  case Negate(tag: Span, expr: Expr)
  case Not(tag: Span, expr: Expr)

  case Literal(tag: Span, value: LiteralType)

  case Grouping(expr: Expr)

  case Variable(tag: Span, name: String)

object Expr:

  def show(expr: Expr): String =
    expr match
      case Add(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} + ${formatNestedExpr(right, show(right))}"
      case Subtract(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} - ${formatNestedExpr(right, show(right))}"
      case Multiply(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} × ${formatNestedExpr(right, show(right))}"
      case Divide(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} ÷ ${formatNestedExpr(right, show(right))}"

      case Negate(_, expr) => s"-${formatNestedExpr(expr, show(expr))}"
      case Not(_, expr)    => s"!${formatNestedExpr(expr, show(expr))}"

      case Greater(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} > ${formatNestedExpr(right, show(right))}"
      case GreaterEqual(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} ≥ ${formatNestedExpr(right, show(right))}"
      case Less(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} < ${formatNestedExpr(right, show(right))}"
      case LessEqual(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} ≤ ${formatNestedExpr(right, show(right))}"
      case Equal(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} == ${formatNestedExpr(right, show(right))}"
      case NotEqual(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} ≠ ${formatNestedExpr(right, show(right))}"

      case Grouping(expr) => s"${formatNestedExpr(expr, show(expr))})"

      case Literal(_, value) =>
        value match
          case _: Unit => "nil"
          case v       => v.toString

      case And(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} and ${formatNestedExpr(right, show(right))}"

      case Or(_, left, right) =>
        s"${formatNestedExpr(left, show(left))} or ${formatNestedExpr(right, show(right))}"

      case Variable(_, name) => name

  private def formatNestedExpr(expr: Expr, exprShow: String): String =
    expr match
      case Literal(_, _) => exprShow
      case _             => s"($exprShow)"

  given Show[Expr] = Show.show(Expr.show)
