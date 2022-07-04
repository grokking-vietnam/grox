package grox

import Token.*

extension (e: Expr)

  def binary(operator: Token[Unit], l: Expr, r: Expr) = l.flatten ::: List(operator) ::: r.flatten

  def flatten: List[Token[Unit]] =
    e match
      case Expr.Add(l, r)      => binary(Plus(()), l, r)
      case Expr.Subtract(l, r) => binary(Minus(()), l, r)
      case Expr.Multiply(l, r) => binary(Star(()), l, r)
      case Expr.Divide(l, r)   => binary(Slash(()), l, r)

      case Expr.Equal(l, r)    => binary(EqualEqual(()), l, r)
      case Expr.NotEqual(l, r) => binary(BangEqual(()), l, r)

      case Expr.Greater(l, r)      => binary(Greater(()), l, r)
      case Expr.GreaterEqual(l, r) => binary(GreaterEqual(()), l, r)
      case Expr.Less(l, r)         => binary(Less(()), l, r)
      case Expr.LessEqual(l, r)    => binary(LessEqual(()), l, r)

      case Expr.Negate(e) => Minus(()) :: e.flatten
      case Expr.Not(e)    => Bang(()) :: e.flatten

      case Expr.Grouping(e) => List(LeftParen(())) ::: e.flatten ::: List(RightParen(()))

      case Expr.Literal(n: Double) => List(Number(n.toString, (())))
      case Expr.Literal(s: String) => List(Number(s, (())))
      case Expr.Literal(true)      => List(True(()))
      case Expr.Literal(false)     => List(False(()))
      case Expr.Literal(())        => List(Null(()))
