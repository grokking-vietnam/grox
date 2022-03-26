package grox

import cats._

object Parser:

  trait ParserAgl[F[_]] {
    def parseToken(tokens: List[Token]): F[Expr]
  }

  given parser[F[_]](
    using ME: MonadError[F, grox.Error],
    A: Applicative[F],
  ): ParserAgl[F] =
    new ParserAgl {

      def parseToken(
        tokens: List[Token]
      ): F[Expr] = parse(tokens).fold(
        err => ME.raiseError(grox.Error.ParserError),
        { case (expr, tokens) => A.pure(expr) },
      )

    }

  enum Error(msg: String, tokens: List[Token]):
    case ExpectExpression(tokens: List[Token]) extends Error("Expect expression", tokens)
    case ExpectClosing(tokens: List[Token]) extends Error("Expect ')' after expression", tokens)

  type ParseResult = Either[Error, (Expr, List[Token])]

  type BinaryOp = Token => Option[(Expr, Expr) => Expr]
  type UnaryOp = Token => Option[Expr => Expr]

  // Parse a single expression and return remaining tokens
  def parse(ts: List[Token]): ParseResult = expression(ts)

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
    case Operator.EqualEqual => Some(Expr.Equal.apply)
    case Operator.BangEqual  => Some(Expr.NotEqual.apply)
    case _                   => None

  val comparisonOp: BinaryOp =
    case Operator.Less         => Some(Expr.Less.apply)
    case Operator.LessEqual    => Some(Expr.LessEqual.apply)
    case Operator.Greater      => Some(Expr.Greater.apply)
    case Operator.GreaterEqual => Some(Expr.GreaterEqual.apply)
    case _                     => None

  val termOp: BinaryOp =
    case Operator.Plus  => Some(Expr.Add.apply)
    case Operator.Minus => Some(Expr.Subtract.apply)
    case _              => None

  val factorOp: BinaryOp =
    case Operator.Star  => Some(Expr.Multiply.apply)
    case Operator.Slash => Some(Expr.Divide.apply)
    case _              => None

  val unaryOp: UnaryOp =
    case Operator.Minus => Some(Expr.Negate.apply)
    case Operator.Bang  => Some(Expr.Not.apply)
    case _              => None

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
      case Literal.Number(l) :: rest  => Right(Expr.Literal(l.toDouble), rest)
      case Literal.Str(l) :: rest     => Right(Expr.Literal(l), rest)
      case Keyword.True :: rest       => Right(Expr.Literal(true), rest)
      case Keyword.False :: rest      => Right(Expr.Literal(false), rest)
      case Keyword.Nil :: rest        => Right(Expr.Literal(null), rest)
      case Operator.LeftParen :: rest => parenBody(rest)
      case _                          => Left(Error.ExpectExpression(tokens))

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody(
    tokens: List[Token]
  ): ParseResult = expression(tokens).flatMap((expr, rest) =>
    rest match
      case Operator.RightParen :: rmn => Right(Expr.Grouping(expr), rmn)
      case _                          => Left(Error.ExpectClosing(rest))
  )

  // Discard tokens until a new expression/statement is found
  def synchronize(tokens: List[Token]): List[Token] =
    tokens match
      case t :: rest =>
        t match
          case Operator.Semicolon => rest
          case Keyword.Class | Keyword.Fun | Keyword.Var | Keyword.For | Keyword.If |
              Keyword.While | Keyword.Print | Keyword.Return =>
            tokens
          case _ => synchronize(rest)
      case Nil => Nil
