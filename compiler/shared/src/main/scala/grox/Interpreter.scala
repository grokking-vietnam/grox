package grox

import scala.util.control.NoStackTrace

import cats.*
import cats.syntax.all.*
import cats.syntax.apply.*

trait Interpreter[F[_]]:
  def evaluate(expr: Expr): F[LiteralType]

object Interpreter:
  def instance[F[_]: MonadThrow]: Interpreter[F] = expr => evaluate(expr).liftTo[F]

  enum RuntimeError(op: Token[Unit], msg: String) extends NoStackTrace:
    override def toString = msg

    case MustBeNumbers(op: Token[Unit]) extends RuntimeError(op, "Operands must be numbers.")
    case MustBeNumbersOrStrings
      extends RuntimeError(Token.Plus(()), "Operands must be two numbers or two strings")
    case DivisionByZero extends RuntimeError(Token.Slash(()), "Division by zerro")

  type EvaluationResult = Either[RuntimeError, LiteralType]
  type Evaluate = (LiteralType, LiteralType) => EvaluationResult

  extension (value: LiteralType)

    def `unary_-` : EvaluationResult =
      value match
        case v: Double => Right(-v)
        case _         => Left(RuntimeError.MustBeNumbers(Token.Minus(())))

    def isTruthy: Boolean =
      value match
        case _: Unit    => false
        case v: Boolean => v
        case _          => true

    def `unary_!` : EvaluationResult = Right(!value.isTruthy)

  def evaluateBinary(
    eval: Evaluate
  )(
    left: Expr,
    right: Expr,
  ): EvaluationResult = (evaluate(left), evaluate(right)).mapN(eval).flatten

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

  def equal(left: LiteralType, right: LiteralType): EvaluationResult = Right(left == right)

  def notEqual(
    left: LiteralType,
    right: LiteralType,
  ): EvaluationResult = equal(left, right).flatMap(r => !r)

  def evaluate(expr: Expr): EvaluationResult =
    expr match
      case Expr.Literal(value) => Right(value)
      case Expr.Grouping(e)    => evaluate(e)
      case Expr.Negate(e)      => evaluate(e).flatMap(res => -res)
      case Expr.Not(e)         => evaluate(e).flatMap(`unary_!`)
      case Expr.Add(l, r)      => evaluateBinary(add)(l, r)
      case Expr.Subtract(l, r) => evaluateBinary(subtract)(l, r)
      case Expr.Multiply(l, r) => evaluateBinary(multiply)(l, r)
      case Expr.Divide(l, r)   => evaluateBinary(divide)(l, r)

      case Expr.Greater(l, r)      => evaluateBinary(greater)(l, r)
      case Expr.GreaterEqual(l, r) => evaluateBinary(greaterOrEqual)(l, r)
      case Expr.Less(l, r)         => evaluateBinary(less)(l, r)
      case Expr.LessEqual(l, r)    => evaluateBinary(lessOrEqual)(l, r)
      case Expr.Equal(l, r)        => evaluateBinary(equal)(l, r)
      case Expr.NotEqual(l, r)     => evaluateBinary(notEqual)(l, r)
