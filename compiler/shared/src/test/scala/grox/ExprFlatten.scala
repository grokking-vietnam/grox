package grox

import Token.*
import Span.*

extension (e: Expr)

  def binary(operator: Token[Span], l: Expr, r: Expr) = l.flatten ::: List(operator) ::: r.flatten

  def flatten: List[Token[Span]] = e match
    case Expr.Add(tag, l, r)      => binary(Plus(tag), l, r)
    case Expr.Subtract(tag, l, r) => binary(Minus(tag), l, r)
    case Expr.Multiply(tag, l, r) => binary(Star(tag), l, r)
    case Expr.Divide(tag, l, r)   => binary(Slash(tag), l, r)

    case Expr.Equal(tag, l, r)    => binary(EqualEqual(tag), l, r)
    case Expr.NotEqual(tag, l, r) => binary(BangEqual(tag), l, r)

    case Expr.Greater(tag, l, r)      => binary(Greater(tag), l, r)
    case Expr.GreaterEqual(tag, l, r) => binary(GreaterEqual(tag), l, r)
    case Expr.Less(tag, l, r)         => binary(Less(tag), l, r)
    case Expr.LessEqual(tag, l, r)    => binary(LessEqual(tag), l, r)

    case Expr.Negate(tag, e) => Minus(tag) :: e.flatten
    case Expr.Not(tag, e)    => Bang(tag) :: e.flatten

    case Expr.Grouping(e) => List(LeftParen(empty)) ::: e.flatten ::: List(RightParen(empty))

    case Expr.Literal(tag, n: Double) => List(Number(n.toString, tag))
    case Expr.Literal(tag, s: String) => List(Number(s, tag))
    case Expr.Literal(tag, true)      => List(True(tag))
    case Expr.Literal(tag, false)     => List(False(tag))
    case Expr.Literal(tag, ())        => List(Null(tag))
