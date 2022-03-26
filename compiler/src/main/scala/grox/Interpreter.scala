package grox

object Interpreter {

  extension (value: Any)

    def toDouble: Double =
      value match
        case v: Double => v
        case _         => ???

    def isTruthy: Boolean =
      value match
        case null       => false
        case v: Boolean => v
        case _          => true

  def evaluate(expr: Expr): Any =
    expr match
      case Expr.Literal(value) =>
        value match
          case v: Double  => v
          case v: String  => v
          case v: Boolean => v
          case null       => null
      case Expr.Grouping(e) => evaluate(e)
      case Expr.Negate(e) =>
        evaluate(e) match
          case v: Double => -v
          case _         => ???
      case Expr.Not(e) => evaluate(e).isTruthy
      case Expr.Add(l, r) =>
        (evaluate(l), evaluate(r)) match
          case (vl: String, vr: String) => vl + vr
          case (vl: Double, vr: Double) => vl + vr
          case _                        => ???
      case Expr.Subtract(l, r) => evaluate(l).toDouble - evaluate(r).toDouble
      case Expr.Multiply(l, r) => evaluate(l).toDouble * evaluate(r).toDouble
      case Expr.Divide(l, r)   => evaluate(l).toDouble / evaluate(r).toDouble

      case Expr.Greater(l, r)      => evaluate(l).toDouble > evaluate(r).toDouble
      case Expr.GreaterEqual(l, r) => evaluate(l).toDouble >= evaluate(r).toDouble
      case Expr.Less(l, r)         => evaluate(l).toDouble < evaluate(r).toDouble
      case Expr.LessEqual(l, r)    => evaluate(Expr.Not(Expr.Greater(l, r)))
      case Expr.Equal(l, r)        => evaluate(l).toDouble == evaluate(r).toDouble
      case Expr.NotEqual(l, r)     => evaluate(Expr.Not(Expr.Equal(l, r)))

}
