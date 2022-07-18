package grox

import scala.util.control.NoStackTrace

import cats.*
import cats.mtl.Stateful
import cats.syntax.all.*
import cats.syntax.apply.*

trait Interpreter[F[_]]:
  def evaluate(expr: Expr): F[LiteralType]

object Interpreter:
  def instance[F[_]: MonadThrow](using S: Stateful[F, Environment]): Interpreter[F] =
    expr => evaluate(expr)

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
    eval: Evaluate
  )(
    left: Expr,
    right: Expr,
  )(
    using S: Stateful[F, Environment]
  ): F[LiteralType] = (evaluate(left), evaluate(right)).mapN(eval).map(_.liftTo[F]).flatten

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

  def evaluate[F[_]: MonadThrow](expr: Expr)(using S: Stateful[F, Environment]): F[LiteralType] =
    expr match
      case Expr.Literal(value) => value.pure[F]
      case Expr.Grouping(e)    => evaluate(e)
      case Expr.Negate(e)      => evaluate(e).flatMap(x => (-x).liftTo[F])
      case Expr.Not(e)         => evaluate(e).flatMap(x => (!x).liftTo[F])
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
      case Expr.And(l, r) =>
        evaluate(l).flatMap(lres => if !lres.isTruthy then lres.pure[F] else evaluate(r))
      case Expr.Or(l, r) =>
        evaluate(l).flatMap(lres => if lres.isTruthy then lres.pure[F] else evaluate(r))
      case Expr.Assign(name, expr) =>
        for
          env <- S.get
          result <- evaluate(expr)
          newEnv <- env
            .assign(name.lexeme, result)
            .left
            .map(_ => RuntimeError.VariableNotFound(name))
            .liftTo[F]
          _ <- S.set(newEnv)
        yield result
      case Expr.Variable(name) =>
        for
          env <- S.get
          result <- env
            .get(name.lexeme)
            .left
            .map(_ => RuntimeError.VariableNotFound(name))
            .liftTo[F]
        yield result
