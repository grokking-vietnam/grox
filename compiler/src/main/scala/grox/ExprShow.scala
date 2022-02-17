package grox

import cats.Show

object ExprShow {
  implicit def exprShow[T]: Show[Expr[T]] = Show.show(expr => Expr.show(expr))
}
