package grox

import scala.util.control.NoStackTrace

import cats.*
import cats.implicits.*

trait Parser[F[_]]:
  def parse[T](tokens: List[Token[T]]): F[Expr]

object Parser:

  def instance[F[_]: MonadThrow]: Parser[F] =
    def parse[T](tokens: List[Token[T]]): F[Expr] =
      parset(tokens).map { case (exp, _) => exp }.liftTo[F]

  enum Error[T](msg: String, tokens: List[Token[T]]) extends NoStackTrace:
    case ExpectExpression(tokens: List[Token[T]]) extends Error("Expect expression", tokens)
    case ExpectClosing(tokens: List[Token[T]]) extends Error("Expect ')' after expression", tokens)

    override def toString: String = msg

  type ParseResult[T] = Either[Error[T], (Expr, List[Token[T]])]

  type BinaryOp[T] = Token[T] => Option[(Expr, Expr) => Expr]
  type UnaryOp[T] = Token[T] => Option[Expr => Expr]

  // Parse a single expression and return remaining tokens
  def parset[T](ts: List[Token[T]]): ParseResult[T] = expression(ts)

  def expression(tokens: List[Token]): ParseResult = equality(tokens)

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)  *
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").
  def binary(
    op: BinaryOp,
    descendant: List[Token] => ParseResult,
  )(
    tokens: List[Token]
  ): ParseResult =
    def matchOp(ts: List[Token], l: Expr): ParseResult =
      ts match
        case token :: rest =>
          op(token) match
            case Some(fn) => descendant(rest).flatMap((r, rmn) => matchOp(rmn, fn(l, r)))
            case None     => Right(l, ts)
        case _ => Right(l, ts)

    descendant(tokens).flatMap((expr, rest) => matchOp(rest, expr))

  val equalityOp: BinaryOp =
    case Operator.EqualEqual(_) => Some(Expr.Equal.apply)
    case Operator.BangEqual(_)  => Some(Expr.NotEqual.apply)
    case _                      => None

  val comparisonOp: BinaryOp =
    case Operator.Less(_)         => Some(Expr.Less.apply)
    case Operator.LessEqual(_)    => Some(Expr.LessEqual.apply)
    case Operator.Greater(_)      => Some(Expr.Greater.apply)
    case Operator.GreaterEqual(_) => Some(Expr.GreaterEqual.apply)
    case _                        => None

  val termOp: BinaryOp =
    case Operator.Plus(_)  => Some(Expr.Add.apply)
    case Operator.Minus(_) => Some(Expr.Subtract.apply)
    case _                 => None

  val factorOp: BinaryOp =
    case Operator.Star(_)  => Some(Expr.Multiply.apply)
    case Operator.Slash(_) => Some(Expr.Divide.apply)
    case _                 => None

  val unaryOp: UnaryOp =
    case Operator.Minus(_) => Some(Expr.Negate.apply)
    case Operator.Bang(_)  => Some(Expr.Not.apply)
    case _                 => None

  def equality = binary(equalityOp, comparison)
  def comparison = binary(comparisonOp, term)
  def term = binary(termOp, factor)
  def factor = binary(factorOp, unary)

  def unary(tokens: List[Token]): ParseResult =
    tokens match
      case token :: rest =>
        unaryOp(token) match
          case Some(fn) => unary(rest).flatMap((expr, rmn) => Right(fn(expr), rmn))
          case None     => primary(tokens)
      case _ => primary(tokens)

  def primary(tokens: List[Token]): ParseResult =
    tokens match
      case Literal.Number(l, s) :: rest  => Right(Expr.Literal(l.toDouble), rest)
      case Literal.Str(l, s) :: rest     => Right(Expr.Literal(l), rest)
      case Keyword.True(_) :: rest       => Right(Expr.Literal(true), rest)
      case Keyword.False(_) :: rest      => Right(Expr.Literal(false), rest)
      case Keyword.Nil(_) :: rest        => Right(Expr.Literal(null), rest)
      case Operator.LeftParen(_) :: rest => parenBody(rest)
      case _                             => Left(Error.ExpectExpression(tokens))

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody(
    tokens: List[Token]
  ): ParseResult = expression(tokens).flatMap((expr, rest) =>
    rest match
      case Operator.RightParen(_) :: rmn => Right(Expr.Grouping(expr), rmn)
      case _                             => Left(Error.ExpectClosing(rest))
  )

  // Discard tokens until a new expression/statement is found
  def synchronize(tokens: List[Token]): List[Token] =
    tokens match
      case t :: rest =>
        t match
          case Operator.Semicolon(_) => rest
          case Keyword.Class(_) | Keyword.Fun(_) | Keyword.Var(_) | Keyword.For(_) | Keyword.If(_) |
              Keyword.While(_) | Keyword.Print(_) | Keyword.Return(_) =>
            tokens
          case _ => synchronize(rest)
      case Nil => Nil
