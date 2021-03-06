package grox

import scala.util.control.NoStackTrace

import cats.*
import cats.syntax.all.*

trait Interpreter[F[_]]:
  def evaluate(env: Environment, expr: Expr): F[LiteralType]

object Interpreter:
  def instance[F[_]: MonadThrow]: Interpreter[F] = (env, expr) => evaluate(env)(expr)

  enum RuntimeError(op: Token[Unit], msg: String) extends NoStackTrace:
    override def toString = msg
    case MustBeNumbers(op: Token[Unit]) extends RuntimeError(op, "Operands must be numbers.")
    case MustBeNumbersOrStrings
      extends RuntimeError(Token.Plus(()), "Operands must be two numbers or two strings")
    case DivisionByZero extends RuntimeError(Token.Slash(()), "Division by zerro")
    case VariableNotFound(op: Token[Unit]) extends RuntimeError(op, "Variable not found")

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

  def evaluateBinary[F[_]: MonadThrow](
    env: Environment
  )(
    eval: Evaluate
  )(
    left: Expr,
    right: Expr,
  ): F[LiteralType] =
    (evaluate(env)(left), evaluate(env)(right)).mapN(eval).map(_.liftTo[F]).flatten

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

  def evaluate[F[_]: MonadThrow](env: Environment)(expr: Expr): F[LiteralType] =
    expr match
      case Expr.Literal(value) => value.pure[F]
      case Expr.Grouping(e)    => evaluate(env)(e)
      case Expr.Negate(e)      => evaluate(env)(e).flatMap(x => (-x).liftTo[F])
      case Expr.Not(e)         => evaluate(env)(e).flatMap(x => (!x).liftTo[F])
      case Expr.Add(l, r)      => evaluateBinary(env)(add)(l, r)
      case Expr.Subtract(l, r) => evaluateBinary(env)(subtract)(l, r)
      case Expr.Multiply(l, r) => evaluateBinary(env)(multiply)(l, r)
      case Expr.Divide(l, r)   => evaluateBinary(env)(divide)(l, r)

      case Expr.Greater(l, r)      => evaluateBinary(env)(greater)(l, r)
      case Expr.GreaterEqual(l, r) => evaluateBinary(env)(greaterOrEqual)(l, r)
      case Expr.Less(l, r)         => evaluateBinary(env)(less)(l, r)
      case Expr.LessEqual(l, r)    => evaluateBinary(env)(lessOrEqual)(l, r)
      case Expr.Equal(l, r)        => evaluateBinary(env)(equal)(l, r)
      case Expr.NotEqual(l, r)     => evaluateBinary(env)(notEqual)(l, r)
      case Expr.And(l, r) =>
        evaluate(env)(l).flatMap(lres => if !lres.isTruthy then lres.pure[F] else evaluate(env)(r))
      case Expr.Or(l, r) =>
        evaluate(env)(l).flatMap(lres => if lres.isTruthy then lres.pure[F] else evaluate(env)(r))
      case Expr.Variable(name) =>
        env
          .get(name.lexeme)
          .left
          .map(_ => RuntimeError.VariableNotFound(name.as(())))
          .liftTo[F]
