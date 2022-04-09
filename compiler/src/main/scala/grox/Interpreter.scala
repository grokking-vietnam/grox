package grox

object Interpreter {
  enum RuntimeError(op: Operator, msg: String):
    case MustBeNumbers(op: Operator) extends RuntimeError(op, "Operands must be numbers.")
    case MustBeNumbersOrStrings extends RuntimeError(Operator.Plus,"Operands must be two numbers or two strings")

  extension (value: Any)

    def isTruthy: Boolean =
      value match
        case null       => false
        case v: Boolean => v
        case _          => true

  def evaluate(expr: Expr): Either[RuntimeError, Any] =
    expr match
      case Expr.Literal(value) =>
        value match
          case v: Double  => Right(v)
          case v: String  => Right(v)
          case v: Boolean => Right(v)
          case null       => null
      case Expr.Grouping(e) => evaluate(e)
      case Expr.Negate(e) =>
        evaluate(e) match
          case Right(v: Double) => Right(-v)
          case _         => Left(RuntimeError.MustBeNumbers(Operator.Minus))
      case Expr.Not(e) => Right(evaluate(e).isTruthy)
      case Expr.Add(l, r) =>
        (evaluate(l), evaluate(r)) match
          case (Right(vl: String), Right(vr: String)) => Right(vl + vr)
          case (Right(vl: Double), Right(vr: Double)) => Right(vl + vr)
          case _                        => Left(RuntimeError.MustBeNumbersOrStrings)
      case Expr.Subtract(l, r) => (evaluate(l), evaluate(r)) match
        case (Right(vl: Double), Right(vr: Double)) => Right(vl - vr)
        case _         => Left(RuntimeError.MustBeNumbers(Operator.Minus)) 
      case Expr.Multiply(l, r) => (evaluate(l), evaluate(r)) match
        case (Right(vl: Double), Right(vr: Double)) => Right(vl * vr)
        case _         => Left(RuntimeError.MustBeNumbers(Operator.Star)) 
      case Expr.Divide(l, r)   => (evaluate(l), evaluate(r)) match
        case (Right(vl: Double), Right(vr: Double)) => Right(vl / vr)
        case _         => Left(RuntimeError.MustBeNumbers(Operator.Slash)) 

      case Expr.Greater(l, r)      => (evaluate(l), evaluate(r)) match
        case (Right(vl: Double), Right(vr: Double)) => Right(vl > vr)
        case _         => Left(RuntimeError.MustBeNumbers(Operator.Greater)) 
      case Expr.GreaterEqual(l, r) => (evaluate(l), evaluate(r)) match
        case (Right(vl: Double), Right(vr: Double)) => Right(vl >= vr)
        case _         => Left(RuntimeError.MustBeNumbers(Operator.GreaterEqual)) 
      case Expr.Less(l, r)         => (evaluate(l), evaluate(r)) match
        case (Right(vl: Double), Right(vr: Double)) => Right(vl < vr)
        case _         => Left(RuntimeError.MustBeNumbers(Operator.Less)) 
      case Expr.LessEqual(l, r)    => (evaluate(l), evaluate(r)) match
        case (Right(vl: Double), Right(vr: Double)) => Right(vl <= vr)
        case _         => Left(RuntimeError.MustBeNumbers(Operator.LessEqual))
      case Expr.Equal(l, r)        => (evaluate(l), evaluate(r)) match
        case (Right(vl: Double), Right(vr: Double)) => Right(vl == vr)
        case _         => Left(RuntimeError.MustBeNumbers(Operator.EqualEqual)) 
      case Expr.NotEqual(l, r)     => (evaluate(l), evaluate(r)) match
        case (Right(vl: Double), Right(vr: Double)) => Right(vl == vr)
        case _         => Left(RuntimeError.MustBeNumbers(Operator.BangEqual)) 

}
