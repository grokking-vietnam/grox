package grox

import scala.annotation.tailrec

import Expr._

object AstPrinter {

  def print(expr: Expr): String =
    expr match
      case e: Literal  => e.value.lexeme
      case e: Unary    => s"(${e.operator.lexeme} ${print(e.expression)})"
      case e: Grouping => s"(group ${print(e.expression)})"
      case e: Binary   => s"(${e.operator.lexeme} ${print(e.left)} ${print(e.right)})"

}
