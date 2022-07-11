package grox

import cats.Show

// We use Unit to represent the absence of a value or nil in the language.
type LiteralType = Double | String | Boolean | Unit

enum Expr[+A]:

  // Binary
  // arithmetic
  case Add(val token: Token[A], left: Expr[A], right: Expr[A])
  case Subtract( val token: Token[A], left: Expr[A], right: Expr[A])
  case Multiply( val token: Token[A], left: Expr[A], right: Expr[A])
  case Divide( val token: Token[A], left: Expr[A], right: Expr[A])

  // comparison
  case Greater( val token: Token[A], left: Expr[A], right: Expr[A])
  case GreaterEqual( val token: Token[A], left: Expr[A], right: Expr[A])
  case Less( val token: Token[A], left: Expr[A], right: Expr[A])
  case LessEqual( val token: Token[A], left: Expr[A], right: Expr[A])
  case Equal( val token: Token[A], left: Expr[A], right: Expr[A])
  case NotEqual( val token: Token[A], left: Expr[A], right: Expr[A])

  // assignment
  case Assign( val token: Token.Identifier[A], value: Expr[A])

  // logic
  case Or( val token: Token[A], left: Expr[A], right: Expr[A])
  case And( val token: Token[A], left: Expr[A], right: Expr[A])

  // Unary
  case Negate( val token: Token[A], expr: Expr[A])
  case Not( val token: Token[A], expr: Expr[A])

  case Literal(value: LiteralType)

  case Grouping( val token: Token[A], expr: Expr[A])

  case Variable( val token: Token.Identifier[A])

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
