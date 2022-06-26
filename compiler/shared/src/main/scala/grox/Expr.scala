package grox

import cats.Show

// We use Unit to represent the absence of a value or nil in the language.
type LiteralType = Double | String | Boolean | Unit

enum Expr[+A](val token: Token[A]):

  // Binary

  // arithmetic
  case Add(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)
  case Subtract(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)
  case Multiply(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)
  case Divide(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)

  // comparison
  case Greater(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)
  case GreaterEqual(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)
  case Less(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)
  case LessEqual(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)
  case Equal(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)
  case NotEqual(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)

  // assignment
  case Assign(override val token: Token.Identifier[A], value: Expr[A]) extends Expr(token)

  // logic
  case Or(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)
  case And(override val token: Token[A], left: Expr[A], right: Expr[A]) extends Expr(token)

  // Unary
  case Negate(override val token: Token[A], expr: Expr[A]) extends Expr(token)
  case Not(override val token: Token[A], expr: Expr[A]) extends Expr(token)

  case Literal(override val token: Token[A], value: LiteralType) extends Expr(token)

  case Grouping(override val token: Token[A], expr: Expr[A]) extends Expr(token)

  case Variable(override val token: Token.Identifier[A]) extends Expr(token)

object Expr:

  def show[A](expr: Expr[A]): String = expr.toString
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


  given [A: Show]: Show[Expr[A]] = Show.show(Expr.show)
