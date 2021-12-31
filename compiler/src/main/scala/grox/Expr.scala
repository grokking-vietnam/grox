package grox

import grox.Expr.Visitor

opaque type UnaryOperator <: Operator = Operator.Bang.type | Operator.Minus.type

enum Expr:

  case Binary(left: Expr, operator: Operator, right: Expr)
  case Grouping(expression: Expr)
  case Unary(operator: UnaryOperator, expression: Expr)
  // TODO: implement correct Literal
  case MyLiteral(value: Literal)

object Expr:

  def dispatch[T](input: Expr, visitor: Visitor[T]): T =
    input match
      case expr: Binary => visitor.visitBinaryExpr(expr)
      case _            => throw Exception("Not Implemented")

  trait Visitor[T]:
    def visitBinaryExpr(expr: Binary): T
    def visitGroupingExpr(expr: Grouping): T
    def visitUnaryExpr(expr: Unary): T
    def visitLiteralExpr(expr: MyLiteral): T
