package grox

import scala.util.control.NoStackTrace

import cats.*
import cats.syntax.all.*

import LiteralType.*

trait Interpreter[F[_]]:
  def evaluate(expr: Expr): F[LiteralType]

object Interpreter:

  def instance[F[_]: MonadThrow](
    using env: Env[F]
  ): Interpreter[F] = new:

    def evaluate(expr: Expr): F[LiteralType] =
      for
        state <- env.state
        result <- evaluateWithState(state)(expr)
      yield result

  enum RuntimeError(location: Span, msg: String) extends NoStackTrace:
    override def toString = msg
    case MustBeNumbers(location: Span) extends RuntimeError(location, "Operands must be numbers.")
    case MustBeNumbersOrStrings(location: Span)
      extends RuntimeError(location, "Operands must be two numbers or two strings")
    case DivisionByZero(location: Span) extends RuntimeError(location, "Division by zerro")
    case VariableNotFound(location: Span, name: String)
      extends RuntimeError(location, "Variable not found")

  type EvaluationResult = Either[RuntimeError, LiteralType]
  type Evaluate = (LiteralType, LiteralType) => EvaluationResult

  extension (value: LiteralType)

    def negate(tag: Span): EvaluationResult = value match
      case v: Double => Right(-v)
      case _         => Left(RuntimeError.MustBeNumbers(tag))

    def `unary_!`: EvaluationResult = Right(!value.isTruthy)

  def evaluateBinary[F[_]: MonadThrow](
    env: State
  )(
    eval: Evaluate
  )(
    left: Expr,
    right: Expr,
  ): F[LiteralType] = (evaluateWithState(env)(left), evaluateWithState(env)(right))
    .mapN(eval)
    .map(_.liftTo[F])
    .flatten

  def add(span: Span)(left: LiteralType, right: LiteralType): EvaluationResult = (left, right) match
    case (l: Double, r: Double) => Right(l + r)
    case (l: String, r: String) => Right(l + r)
    case (_, _)                 => Left(RuntimeError.MustBeNumbersOrStrings(span))

  def subtract(span: Span)(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l - r)
      case _                      => Left(RuntimeError.MustBeNumbers(span))

  def multiply(span: Span)(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l * r)
      case _                      => Left(RuntimeError.MustBeNumbers(span))

  def divide(span: Span)(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) =>
        if (r != 0) Right(l / r)
        else Left(RuntimeError.DivisionByZero(span))
      case _ => Left(RuntimeError.MustBeNumbers(span))

  def greater(span: Span)(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l > r)
      case _                      => Left(RuntimeError.MustBeNumbers(span))

  def greaterOrEqual(span: Span)(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l >= r)
      case _                      => Left(RuntimeError.MustBeNumbers(span))

  def less(span: Span)(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l < r)
      case _                      => Left(RuntimeError.MustBeNumbers(span))

  def lessOrEqual(span: Span)(left: LiteralType, right: LiteralType): EvaluationResult =
    (left, right) match
      case (l: Double, r: Double) => Right(l <= r)
      case _                      => Left(RuntimeError.MustBeNumbers(span))

  def equal(left: LiteralType, right: LiteralType): EvaluationResult = Right(left == right)

  def notEqual(
    left: LiteralType,
    right: LiteralType,
  ): EvaluationResult = equal(left, right).flatMap(r => !r)

  def evaluateWithState[F[_]: MonadThrow](env: State)(expr: Expr): F[LiteralType] = expr match
    case Expr.Literal(_, value)   => value.pure[F]
    case Expr.Grouping(e)         => evaluateWithState(env)(e)
    case Expr.Negate(tag, e)      => evaluateWithState(env)(e).flatMap(x => x.negate(tag).liftTo[F])
    case Expr.Not(_, e)           => evaluateWithState(env)(e).flatMap(x => !x.liftTo[F])
    case Expr.Add(tag, l, r)      => evaluateBinary(env)(add(tag))(l, r)
    case Expr.Subtract(tag, l, r) => evaluateBinary(env)(subtract(tag))(l, r)
    case Expr.Multiply(tag, l, r) => evaluateBinary(env)(multiply(tag))(l, r)
    case Expr.Divide(tag, l, r)   => evaluateBinary(env)(divide(tag))(l, r)

    case Expr.Greater(tag, l, r)      => evaluateBinary(env)(greater(tag))(l, r)
    case Expr.GreaterEqual(tag, l, r) => evaluateBinary(env)(greaterOrEqual(tag))(l, r)
    case Expr.Less(tag, l, r)         => evaluateBinary(env)(less(tag))(l, r)
    case Expr.LessEqual(tag, l, r)    => evaluateBinary(env)(lessOrEqual(tag))(l, r)
    case Expr.Equal(tag, l, r)        => evaluateBinary(env)(equal)(l, r)
    case Expr.NotEqual(tag, l, r)     => evaluateBinary(env)(notEqual)(l, r)
    case Expr.And(_, l, r) => evaluateWithState(env)(l).flatMap(lres =>
        if !lres.isTruthy then lres.pure[F] else evaluateWithState(env)(r)
      )
    case Expr.Or(_, l, r) => evaluateWithState(env)(l).flatMap(lres =>
        if lres.isTruthy then lres.pure[F] else evaluateWithState(env)(r)
      )
    case Expr.Variable(tag, name) => env
        .get(name)
        .left
        .map(_ => RuntimeError.VariableNotFound(tag, name))
        .liftTo[F]
