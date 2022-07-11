package grox

import cats.Show

// We use Unit to represent the absence of a value or nil in the language.
type LiteralType = Double | String | Boolean | Unit

enum Expr:

  // Binary
  // arithmetic
  case Add(val token: Span, left: Expr, right: Expr)
  case Subtract(val token: A, left: Expr, right: Expr)
  case Multiply(val token: A, left: Expr, right: Expr)
  case Divide(val token: A, left: Expr, right: Expr)

  // comparison
  case Greater(val token: A, left: Expr, right: Expr)
  case GreaterEqual(val token: A, left: Expr, right: Expr)
  case Less(val token: A, left: Expr, right: Expr)
  case LessEqual(val token: A, left: Expr, right: Expr)
  case Equal(val token: A, left: Expr, right: Expr)
  case NotEqual(val token: A, left: Expr, right: Expr)

  // assignment
  case Assign(val token: A, val name: String, val value: Expr)

  // logic
  case Or(val token: A, left: Expr, right: Expr)
  case And(val token: A, left: Expr, right: Expr)

  // Unary
  case Negate(val token: A, expr: Expr)
  case Not(val token: A, expr: Expr)

  case Literal(val token: A, value: LiteralType)

  case Grouping(expr: Expr)

  case Variable(val token: A, name: String)

object Expr:

  def show(expr: Expr): String = expr.toString
  // expr match
  //   case Add(left, right) =>
  //     s"${formatNestedExpr(left, show(left))} + ${formatNestedExpr(right, show(right))}"
  //   case Subtract(left, right) =>
  //     s"${formatNestedExpr(left, show(left))} - ${formatNestedExpr(right, show(right))}"
  //   case Multiply(left, right) =>
  //     s"${formatNestedExpr(left, show(left))} × ${formatNestedExpr(right, show(right))}"
  //   case Divide(left, right) =>
  //     s"${formatNestedExpr(left, show(left))} ÷ ${formatNestedExpr(right, show(right))}"
  //
  //   case Negate(expr) => s"-${formatNestedExpr(expr, show(expr))}"
  //   case Not(expr)    => s"!${formatNestedExpr(expr, show(expr))}"
  //
  //   case Greater(left, right) =>
  //     s"${formatNestedExpr(left, show(left))} > ${formatNestedExpr(right, show(right))}"
  //   case GreaterEqual(left, right) =>
  //     s"${formatNestedExpr(left, show(left))} ≥ ${formatNestedExpr(right, show(right))}"
  //   case Less(left, right) =>
  //     s"${formatNestedExpr(left, show(left))} < ${formatNestedExpr(right, show(right))}"
  //   case LessEqual(left, right) =>
  //     s"${formatNestedExpr(left, show(left))} ≤ ${formatNestedExpr(right, show(right))}"
  //   case Equal(left, right) =>
  //     s"${formatNestedExpr(left, show(left))} == ${formatNestedExpr(right, show(right))}"
  //   case NotEqual(left, right) =>
  //     s"${formatNestedExpr(left, show(left))} ≠ ${formatNestedExpr(right, show(right))}"
  //
  //   case Grouping(expr) => s"${formatNestedExpr(expr, show(expr))})"
  //
  // case Literal(value) =>
  // value match
  // case _: Unit => "nil"
  // case v       => v.toString

  // case And(left, right) =>
  //   s"${formatNestedExpr(left, show(left))} and ${formatNestedExpr(right, show(right))}"
  //
  // case Or(left, right) =>
  //   s"${formatNestedExpr(left, show(left))} or ${formatNestedExpr(right, show(right))}"
  //
  // case Assign(name, value) => s"${name.lexeme} = ${formatNestedExpr(value, show(value))}"
  //
  // case Variable(name) => name.lexeme

  // private def formatNestedExpr(expr: Expr, exprShow: String): String =
  //   expr match
  //     case Literal(_) => exprShow
  //     case _          => s"($exprShow)"

  given [Show]: Show[Expr] = Show.show(Expr.show)
