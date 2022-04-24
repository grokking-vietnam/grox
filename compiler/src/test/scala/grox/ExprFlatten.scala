package grox

extension (e: Expr)

  def binary(operator: Token, l: Expr, r: Expr) = l.flatten ::: List(operator) ::: r.flatten

  def flatten: List[Token] =
    e match
      case Expr.Add(l, r)      => binary(Operator.Plus, l, r)
      case Expr.Subtract(l, r) => binary(Operator.Minus, l, r)
      case Expr.Multiply(l, r) => binary(Operator.Star, l, r)
      case Expr.Divide(l, r)   => binary(Operator.Slash, l, r)

      case Expr.Equal(l, r)    => binary(Operator.EqualEqual, l, r)
      case Expr.NotEqual(l, r) => binary(Operator.BangEqual, l, r)

      case Expr.Greater(l, r)      => binary(Operator.Greater, l, r)
      case Expr.GreaterEqual(l, r) => binary(Operator.GreaterEqual, l, r)
      case Expr.Less(l, r)         => binary(Operator.Less, l, r)
      case Expr.LessEqual(l, r)    => binary(Operator.LessEqual, l, r)

      case Expr.Negate(e) => Operator.Minus :: e.flatten
      case Expr.Not(e)    => Operator.Bang :: e.flatten

      case Expr.Grouping(e) => List(Operator.LeftParen) ::: e.flatten ::: List(Operator.RightParen)

      case Expr.Literal(n: Double) => List(Literal.Number(n.toString))
      case Expr.Literal(s: String) => List(Literal.Number(s))
      case Expr.Literal(true)      => List(Keyword.True)
      case Expr.Literal(false)     => List(Keyword.False)
      case Expr.Literal(null)      => List(Keyword.Nil)
