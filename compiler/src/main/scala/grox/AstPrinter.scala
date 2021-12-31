package grox

import Expr._
class AstPrinter extends Expr.Visitor[String]:
  def parenthesize(name: String, expr: Expr*): String = s"($name ${expr})"

  def visitBinaryExpr(
    expr: Expr.Binary
  ): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)

  def visitGroupingExpr(expr: Grouping): String = parenthesize("group", expr.expression)
  def visitUnaryExpr(expr: Unary): String = parenthesize(expr.operator.lexeme, expr.expression)
  def visitLiteralExpr(expr: MyLiteral): String = expr.toString()

object AstPrinter {
  println(Expr.dispatch(Binary(Literal.Number("1"))))
}
