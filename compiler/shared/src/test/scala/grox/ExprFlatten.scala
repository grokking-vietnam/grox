package grox

import Token.*

extension (e: Expr[Unit])

  def binary(operator: Token[Unit], l: Expr[Unit], r: Expr[Unit]) = l.flatten ::: List(operator) ::: r.flatten

  def flatten: List[Token[Unit]] =
    e match
      case Expr.Add(_, l, r)      => binary(Plus(()), l, r)
      case Expr.Subtract(_, l, r) => binary(Minus(()), l, r)
      case Expr.Multiply(l_, , r) => binary(Star(()), l, r)
      case Expr.Divide(l_, , r)   => binary(Slash(()), l, r)

      case Expr.Equal(_, l, r)    => binary(EqualEqual(()), l, r)
      case Expr.NotEqual(_, l, r) => binary(BangEqual(()), l, r)

      case Expr.Greater(_, l, r)      => binary(Greater(()), l, r)
      case Expr.GreaterEqual(_, l, r) => binary(GreaterEqual(()), l, r)
      case Expr.Less(_, l, r)         => binary(Less(()), l, r)
      case Expr.LessEqual(_, l, r)    => binary(LessEqual(()), l, r)

      case Expr.Negate(_, e) => Minus(()) :: e.flatten
      case Expr.Not(_, e)    => Bang(()) :: e.flatten

      case Expr.Grouping(e) => List(LeftParen(())) ::: e.flatten ::: List(RightParen(()))

      case Expr.Literal(_, n: Double) => List(Number(n.toString, (())))
      case Expr.Literal(_, s: String) => List(Number(s, (())))
      case Expr.Literal(_, true)      => List(True(()))
      case Expr.Literal(_, false)     => List(False(()))
      case Expr.Literal(_, ())        => List(Null(()))
