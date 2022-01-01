package grox

import grox.Expr.Visitor

enum Expr:

  case Binary(left: Expr, operator: Token.Operator, right: Expr)
  case Grouping(expression: Expr)
  case Unary(operator: Token.Operator, expression: Expr)
  case Literal(value: Token.Literal)

object Expr:

  // TODO: Should we use visitor pattern?
  def dispatch[T](input: Expr, visitor: Visitor[T]): T =
    input match
      case expr: Literal  => visitor.visitLiteralExpr(expr)
      case expr: Unary    => visitor.visitUnaryExpr(expr)
      case expr: Grouping => visitor.visitGroupingExpr(expr)
      case expr: Binary   => visitor.visitBinaryExpr(expr)

  trait Visitor[+T]:
    def visitBinaryExpr(expr: Binary): T
    def visitGroupingExpr(expr: Grouping): T
    def visitUnaryExpr(expr: Unary): T
    def visitLiteralExpr(expr: Literal): T
