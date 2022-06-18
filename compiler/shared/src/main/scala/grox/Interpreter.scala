package grox

import cats.syntax.apply.*
import cats.implicits.*

object Interpreter:

  enum RuntimeError(op: Token[Unit], msg: String):
    case MustBeNumbers(op: Token[Unit]) extends RuntimeError(op, "Operands must be numbers.")
    case MustBeNumbersOrStrings
      extends RuntimeError(Token.Plus(()), "Operands must be two numbers or two strings")
    case DivisionByZero extends RuntimeError(Token.Slash(()), "Division by zerro")

  type EvaluationResult = Either[RuntimeError, GlobalLiteralType]
  type GlobalLiteralType = (LiteralType, Global)
  type Evaluate = (GlobalLiteralType, GlobalLiteralType) => EvaluationResult

  case class Global(environment: Environment) {
    def defineVar(token: Token, value: LiteralType): Global = copy(environment = environment.define(token, value))
  }

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

  def add(left: GlobalLiteralType, right: GlobalLiteralType): EvaluationResult =
    (left, right) match
      case (l: (Double, _), r: (Double, _)) => Right((l._1 + r._1) -> l._2)
      case (l: (String, _), r: (String, _)) => Right((l._1 + r._1) -> l._2)
      case _                      => Left(RuntimeError.MustBeNumbersOrStrings)

  def subtract(left: GlobalLiteralType, right: GlobalLiteralType): EvaluationResult =
    (left, right) match
      case (l: (Double, _), r: (Double, _)) => Right((l._1 - r._1) -> l._2)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Minus(())))

  def multiply(left: GlobalLiteralType, right: GlobalLiteralType): EvaluationResult =
    (left, right) match
      case (l: (Double, _), r: (Double, _)) => Right((l._1 * r._1) -> l._2)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Star(())))

  def divide(left: GlobalLiteralType, right: GlobalLiteralType): EvaluationResult =
    (left, right) match
      case (l: (Double, _), r: (Double, _)) =>
        if (r._1 != 0)
          Right((l._1 / r._1) -> l._2)
        else
          Left(RuntimeError.DivisionByZero)
      case _ => Left(RuntimeError.MustBeNumbers(Token.Slash(())))

  def greater(left: GlobalLiteralType, right: GlobalLiteralType): EvaluationResult =
    (left, right) match
      case (l: (Double, _), r: (Double, _)) => Right((l._1 > r._1) -> l._2)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Greater(())))

  def greaterOrEqual(left: GlobalLiteralType, right: GlobalLiteralType): EvaluationResult =
    (left, right) match
      case (l: (Double, _), r: (Double, _)) => Right((l._1 >= r._1) -> l._2)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.GreaterEqual(())))

  def less(left: GlobalLiteralType, right: GlobalLiteralType): EvaluationResult =
    (left, right) match
      case (l: (Double, _), r: (Double, _2)) => Right((l._1 < r._1) -> l._2)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.Less(())))

  def lessOrEqual(left: GlobalLiteralType, right: GlobalLiteralType): EvaluationResult =
    (left, right) match
      case (l: (Double, _), r: (Double, _)) => Right((l._1 <= r._1) -> l._2)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.LessEqual(())))

  def equal(left: GlobalLiteralType, right: GlobalLiteralType): EvaluationResult =
    (left, right) match
      case (l: (Double, _), r: (Double, _)) => Right((l._1 == r._1) -> l._2)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.EqualEqual(())))

  def notEqual(left: GlobalLiteralType, right: GlobalLiteralType): EvaluationResult =
    (left, right) match
      case (l: (Double, _), r: (Double, _)) => Right((l != r) -> l._2)
      case _                      => Left(RuntimeError.MustBeNumbers(Token.BangEqual(())))

  def evaluate(expr: Expr, global: Global): EvaluationResult =
    expr match
      case Expr.Literal(value) => Right(value -> global)
      case Expr.Grouping(e)    => evaluate(e, global)
      case Expr.Negate(e) =>
        evaluate(e, global) match
          case Right((v: Double, glb)) => Right(-v -> glb)
          case _                => Left(RuntimeError.MustBeNumbers(Token.Minus(())))
      case Expr.Not(e)         => evaluate(e, global).map(res => isTruthy(res._1) -> res._2)
      case Expr.Add(l, r)      => evaluateBinary(add)(evaluate(l, global), evaluate(r, global))
      case Expr.Subtract(l, r) => evaluateBinary(subtract)(evaluate(l, global), evaluate(r, global))
      case Expr.Multiply(l, r) => evaluateBinary(multiply)(evaluate(l, global), evaluate(r, global))
      case Expr.Divide(l, r)   => evaluateBinary(divide)(evaluate(l, global), evaluate(r, global))

      case Expr.Greater(l, r)      => evaluateBinary(greater)(evaluate(l, global), evaluate(r, global))
      case Expr.GreaterEqual(l, r) => evaluateBinary(greaterOrEqual)(evaluate(l, global), evaluate(r, global))
      case Expr.Less(l, r)         => evaluateBinary(less)(evaluate(l, global), evaluate(r, global))
      case Expr.LessEqual(l, r)    => evaluateBinary(lessOrEqual)(evaluate(l, global), evaluate(r, global))
      case Expr.Equal(l, r)        => evaluateBinary(equal)(evaluate(l, global), evaluate(r, global))
      case Expr.NotEqual(l, r)     => evaluateBinary(notEqual)(evaluate(l, global), evaluate(r, global))

  private def visitExpressionStmt[A](stmt: Stmt.Expression[A], global: Global): EvaluationResult =
    evaluate(stmt.expr, global)

  private def visitPrintStmt[A](stmt: Stmt.Print[A], global: Global): EvaluationResult =
    evaluate(stmt.expr, global).fold(
      _ => identity,
      l => println(l._1.show)
    )

  private def visitVarStmt[A](stmt: Stmt.Var[A], global: Global): EvaluationResult =
    stmt.init.map(evaluate(_, global)).getOrElse(Right(Null)).map(defineVar(_))
