package grox

import scala.util.control.NoStackTrace

import cats.*
import cats.implicits.*

trait Parser[F[_]]:
  def parse[T](tokens: List[Token[T]]): F[Expr]

object Parser:

  import Token.*

  def instance[F[_]: MonadThrow]: Parser[F] =
    new Parser[F]:

      def parse[T](
        tokens: List[Token[T]]
      ): F[Expr] = Parser.parse(tokens).map { case (exp, _) => exp }.liftTo[F]

  enum Error[T](msg: String, tokens: List[Token[T]]) extends NoStackTrace:
    case ExpectExpression(tokens: List[Token[T]]) extends Error("Expect expression", tokens)
    case ExpectClosing(tokens: List[Token[T]]) extends Error("Expect ')' after expression", tokens)

    override def toString: String = msg

  type ParseResult[A] = Either[Error[A], (Expr, List[Token[A]])]

  type BinaryOp[A] = Token[A] => Option[(Expr, Expr) => Expr]
  type UnaryOp[A] = Token[A] => Option[Expr => Expr]

  // Parse a single expression and return remaining tokens
  def parse[A](ts: List[Token[A]]): ParseResult[A] = expression[A](ts)

  def expression[A](tokens: List[Token[A]]): ParseResult[A] = equality(tokens)

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)  *
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").
  def binary[A](
    op: BinaryOp[A],
    descendant: List[Token[A]] => ParseResult[A],
  )(
    tokens: List[Token[A]]
  ): ParseResult[A] =
    def matchOp(ts: List[Token[A]], l: Expr): ParseResult[A] =
      ts match
        case token :: rest =>
          op(token) match
            case Some(fn) => descendant(rest).flatMap((r, rmn) => matchOp(rmn, fn(l, r)))
            case None     => Right(l, ts)
        case _ => Right(l, ts)

    descendant(tokens).flatMap((expr, rest) => matchOp(rest, expr))

  def equalityOp[A]: BinaryOp[A] =
    case EqualEqual(_) => Some(Expr.Equal.apply)
    case BangEqual(_)  => Some(Expr.NotEqual.apply)
    case _             => None

  def comparisonOp[A]: BinaryOp[A] =
    case Less(_)         => Some(Expr.Less.apply)
    case LessEqual(_)    => Some(Expr.LessEqual.apply)
    case Greater(_)      => Some(Expr.Greater.apply)
    case GreaterEqual(_) => Some(Expr.GreaterEqual.apply)
    case _               => None

  def termOp[A]: BinaryOp[A] =
    case Plus(_)  => Some(Expr.Add.apply)
    case Minus(_) => Some(Expr.Subtract.apply)
    case _        => None

  def factorOp[A]: BinaryOp[A] =
    case Star(_)  => Some(Expr.Multiply.apply)
    case Slash(_) => Some(Expr.Divide.apply)
    case _        => None

  def unaryOp[A]: UnaryOp[A] =
    case Minus(_) => Some(Expr.Negate.apply)
    case Bang(_)  => Some(Expr.Not.apply)
    case _        => None

  def equality[A] = binary[A](equalityOp, comparison)
  def comparison[A] = binary[A](comparisonOp, term)
  def term[A] = binary[A](termOp, factor)
  def factor[A] = binary[A](factorOp, unary)

  def unary[A](tokens: List[Token[A]]): ParseResult[A] =
    tokens match
      case token :: rest =>
        unaryOp(token) match
          case Some(fn) => unary(rest).flatMap((expr, rmn) => Right(fn(expr), rmn))
          case None     => primary(tokens)
      case _ => primary(tokens)

  def primary[A](tokens: List[Token[A]]): ParseResult[A] =
    tokens match
      case Number(l, s) :: rest => Right(Expr.Literal(l.toDouble), rest)
      case Str(l, s) :: rest    => Right(Expr.Literal(l), rest)
      case True(_) :: rest      => Right(Expr.Literal(true), rest)
      case False(_) :: rest     => Right(Expr.Literal(false), rest)
      case Null(_) :: rest      => Right(Expr.Literal(null), rest)
      case LeftParen(_) :: rest => parenBody[A](rest)
      case _                    => Left(Error.ExpectExpression(tokens))

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody[A](
    tokens: List[Token[A]]
  ): ParseResult[A] = expression(tokens).flatMap((expr, rest) =>
    rest match
      case RightParen(_) :: rmn => Right(Expr.Grouping(expr), rmn)
      case _                    => Left(Error.ExpectClosing(rest))
  )

  // Discard tokens until a new expression/statement is found
  def synchronize[A](tokens: List[Token[A]]): List[Token[A]] =
    tokens match
      case t :: rest =>
        t match
          case Semicolon(_) => rest
          case Class(_) | Fun(_) | Var(_) | For(_) | If(_) | While(_) | Print(_) | Return(_) =>
            tokens
          case _ => synchronize(rest)
      case List() => List()
