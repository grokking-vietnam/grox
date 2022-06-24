package grox

import cats.syntax.apply.*
import cats.*
import cats.syntax.all.*
import scala.util.control.NoStackTrace

trait Interpreter[F[_]]:
  def evaluate(expr: Expr): F[LiteralType]

object Interpreter:
  def instance[F[_]: MonadThrow]: Interpreter[F] = expr => evaluate(expr).liftTo[F]

  enum RuntimeError(op: Token[Unit], msg: String) extends NoStackTrace:
    case MustBeNumbers(op: Token[Unit]) extends RuntimeError(op, "Operands must be numbers.")
    case MustBeNumbersOrStrings
      extends RuntimeError(Token.Plus(()), "Operands must be two numbers or two strings")
    case DivisionByZero extends RuntimeError(Token.Slash(()), "Division by zerro")

  type EvaluationResult = Either[RuntimeError, LiteralType]
  type Evaluate = (LiteralType, LiteralType) => EvaluationResult

  extension (value: LiteralType)

    def isTruthy: LiteralType =
      value match
        case null       => false
        case v: Boolean => v
        case _          => true

  def evaluateBinary(
    eval: Evaluate
  )(
    left: EvaluationResult,
    right: EvaluationResult,
  ): EvaluationResult = (left, right).mapN(eval).flatten

  def add(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l + r)
      case (l: String, r: String) => Right(l + r)
      case _                      => Left(RuntimeError.MustBeNumbersOrStrings)

  def subtract(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l - r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Minus(())))

  def multiply(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l * r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Star(())))

  def divide(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) =>
        if (r != 0)
          Right(l / r)
        else
          Left(RuntimeError.DivisionByZero)
      case _ => Left(RuntimeError.MustBeNumbers(Token.Slash(())))

  def greater(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l > r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Greater(())))

  def greaterOrEqual(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l >= r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.GreaterEqual(())))

  def less(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l < r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Less(())))

  def lessOrEqual(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l <= r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.LessEqual(())))

  def equal(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l == r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.EqualEqual(())))

  def notEqual(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l != r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.BangEqual(())))

  def evaluate(expr: Expr): EvaluationResult =
    expr match
      case Expr.Literal(value) => Right(value)
      case Expr.Grouping(e)    => evaluate(e)
      case Expr.Negate(e) =>
        evaluate(e) match
          case Right(v: Double) => Right(-v)
          case _                => Left(RuntimeError.MustBeNumbers(Token.Minus(())))
      case Expr.Not(e)         => evaluate(e).map(isTruthy)
      case Expr.Add(l, r)      => evaluateBinary(add)(evaluate(l), evaluate(r))
      case Expr.Subtract(l, r) => evaluateBinary(subtract)(evaluate(l), evaluate(r))
      case Expr.Multiply(l, r) => evaluateBinary(multiply)(evaluate(l), evaluate(r))
      case Expr.Divide(l, r)   => evaluateBinary(divide)(evaluate(l), evaluate(r))

      case Expr.Greater(l, r)      => evaluateBinary(greater)(evaluate(l), evaluate(r))
      case Expr.GreaterEqual(l, r) => evaluateBinary(greaterOrEqual)(evaluate(l), evaluate(r))
      case Expr.Less(l, r)         => evaluateBinary(less)(evaluate(l), evaluate(r))
      case Expr.LessEqual(l, r)    => evaluateBinary(lessOrEqual)(evaluate(l), evaluate(r))
      case Expr.Equal(l, r)        => evaluateBinary(equal)(evaluate(l), evaluate(r))
      case Expr.NotEqual(l, r)     => evaluateBinary(notEqual)(evaluate(l), evaluate(r))
