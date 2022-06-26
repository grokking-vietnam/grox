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
    case MustBeNumbersOrStrings(op: Token[Unit])
      extends RuntimeError(op, "Operands must be numbers or strings.")
    case DivisionByZero extends RuntimeError(Token.Slash(()), "Division by zerro")

  type EvaluationResult = Either[RuntimeError, LiteralType]
  type Evaluate = (LiteralType, LiteralType) => EvaluationResult

  extension (value: LiteralType)

    def `unary_-`: EvaluationResult =
      value match
        case v: Double => Right(-v)
        case _         => ??? // Left(RuntimeError.MustBeNumbers(Token.Minus(())))

    def isTruthy: Boolean =
      value match
        case _: Unit    => false
        case v: Boolean => v
        case _          => true

    def `unary_!`: EvaluationResult = Right(!value.isTruthy)

  def evaluateBinary[A](
    eval: Evaluate
  )(
    left: Expr[A],
    right: Expr[A],
  ): EvaluationResult = (evaluate(left), evaluate(right)).mapN(eval).flatten

  def add[A](left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l + r)
      case (l: String, r: String) => Right(l + r)
      case _                      => ??? // Left(RuntimeError.MustBeNumbersOrStrings)

  def subtract[A](left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l - r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Minus(())))

  def multiply[A](left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l * r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Star(())))

  def divide[A](left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) =>
        if (r != 0)
          Right(l / r)
        else
          Left(RuntimeError.DivisionByZero)
      case _ => Left(RuntimeError.MustBeNumbers(Token.Slash(())))

  def greater[A](left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l > r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Greater(())))

  def greaterOrEqual[A](left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l >= r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.GreaterEqual(())))

  def less[A](left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l < r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Less(())))

  def lessOrEqual[A](left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l <= r)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.LessEqual(())))

  def equal[A](left: LiteralType, right: LiteralType): EvaluationResult = Right(left == right)

  def notEqual[A](
    left: LiteralType,
    right: LiteralType,
  ): EvaluationResult = equal(left, right).flatMap(r => !r)

  def evaluate[A](expr: Expr[A]): EvaluationResult =
    expr match
      case Expr.Literal(_, value) => Right(value)
      case Expr.Grouping(_, e)    => evaluate(e)
      case Expr.Negate(_, e)      => evaluate(e).flatMap(res => -res)
      case Expr.Not(_, e)         => evaluate(e).flatMap(`unary_!`)
      case Expr.Add(_, l, r)      => evaluateBinary(add)(l, r)
      case Expr.Subtract(_, l, r) => evaluateBinary(subtract)(l, r)
      case Expr.Multiply(_, l, r) => evaluateBinary(multiply)(l, r)
      case Expr.Divide(_, l, r)   => evaluateBinary(divide)(l, r)

      case Expr.Greater(_, l, r)      => evaluateBinary(greater)(l, r)
      case Expr.GreaterEqual(_, l, r) => evaluateBinary(greaterOrEqual)(l, r)
      case Expr.Less(_, l, r)         => evaluateBinary(less)(l, r)
      case Expr.LessEqual(_, l, r)    => evaluateBinary(lessOrEqual)(l, r)
      case Expr.Equal(_, l, r)        => evaluateBinary(equal)(l, r)
      case Expr.NotEqual(_, l, r)     => evaluateBinary(notEqual)(l, r)
      case Expr.And(_, l, r) =>
        evaluate(l).flatMap(lres => if !lres.isTruthy then Right(lres) else evaluate(r))
      case Expr.Or(_, l, r) =>
        evaluate(l).flatMap(lres => if lres.isTruthy then Right(lres) else evaluate(r))
      case Expr.Assign(name, value) => ???
      case Expr.Variable(name)      => ???
