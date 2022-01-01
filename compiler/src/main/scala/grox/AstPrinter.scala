package grox

import Expr._

class AstPrinterVisitor extends Expr.Visitor[String]:

  def parenthesize(name: String, exprs: Expr*): String =
    val exprStr = exprs.map(expr => dispatch(expr, new AstPrinterVisitor)).mkString(" ")
    s"($name ${exprStr})"

  def visitBinaryExpr(
    expr: Expr.Binary
  ): String = parenthesize(expr.operator.lexeme, expr.left, expr.right)

  def visitGroupingExpr(expr: Grouping): String = parenthesize("group", expr.expression)
  def visitUnaryExpr(expr: Unary): String = parenthesize(expr.operator.lexeme, expr.expression)
  def visitLiteralExpr(expr: Literal): String = expr.value.lexeme

object AstPrinter extends App {
  def print(expr: Expr): String = dispatch(expr, new AstPrinterVisitor)
}
