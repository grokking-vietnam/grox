package grox

object Parser:

  enum Error(msg: String):
    case Unexpected(t: Token) extends Error(s"Unexpected '${t.lexeme}'")
    case ExpectExpression extends Error("Expect expression")
    case ExpectClosing extends Error("Expect ')' after expression")

  type ExprTokens = (Either[Error, Expr], List[Token])

  // Parse a single expression and return remaining tokens
  def parse(ts: List[Token]): ExprTokens = expression(ts)

  def expression(tokens: List[Token]): ExprTokens = equality(tokens)

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)*
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").
  def binary(
    operators: List[Operator],
    descendant: List[Token] => ExprTokens,
  )(
    tokens: List[Token]
  ): ExprTokens =
    def matchOp(ts: List[Token], l: Expr): ExprTokens =
      ts match
        case token :: rest if operators.contains(token) =>
          descendant(rest) match
            case (Left(e), rest) => (Left(e), rest)
            case (Right(r), remaining) =>
              makeBinary(token, l, r) match
                case None       => (Left(Error.Unexpected(token)), remaining)
                case Some(expr) => matchOp(remaining, expr)
        case _ => (Right(l), ts)

    descendant(tokens) match
      case (Left(e), rest)     => (Left(e), rest)
      case (Right(expr), rest) => matchOp(rest, expr)

  def makeBinary(token: Token, left: Expr, right: Expr): Option[Expr] =
    token match
      case Operator.EqualEqual   => Some(Expr.Equal(left, right))
      case Operator.BangEqual    => Some(Expr.NotEqual(left, right))
      case Operator.Less         => Some(Expr.Less(left, right))
      case Operator.LessEqual    => Some(Expr.LessEqual(left, right))
      case Operator.Greater      => Some(Expr.Greater(left, right))
      case Operator.GreaterEqual => Some(Expr.GreaterEqual(left, right))
      case Operator.Plus         => Some(Expr.Add(left, right))
      case Operator.Minus        => Some(Expr.Subtract(left, right))
      case Operator.Star         => Some(Expr.Multiply(left, right))
      case Operator.Slash        => Some(Expr.Divide(left, right))
      case _                     => None

  val equalityOperators = List(
    Operator.EqualEqual,
    Operator.BangEqual,
  )

  val comparisonOperators = List(
    Operator.Less,
    Operator.LessEqual,
    Operator.Greater,
    Operator.GreaterEqual,
  )

  val termOperators = List(Operator.Plus, Operator.Minus)
  val factorOperators = List(Operator.Star, Operator.Slash)
  val unaryOperators = List(Operator.Minus, Operator.Bang)

  def equality = binary(equalityOperators, comparison)
  def comparison = binary(comparisonOperators, term)
  def term = binary(termOperators, factor)
  def factor = binary(factorOperators, unary)

  def unary(tokens: List[Token]): ExprTokens =
    tokens match
      case token :: rest if unaryOperators.contains(token) =>
        unary(rest) match
          case (Left(e), rest) => (Left(e), rest)
          case (Right(expr), rest) => // (makeUnary(token, expr), rest)
            makeUnary(token, expr) match
              case Some(expr) => (Right(expr), rest)
              case None       => (Left(Error.Unexpected(token)), rest)
      case ts => primary(ts)

  def makeUnary(token: Token, expr: Expr): Option[Expr] =
    token match
      case Operator.Minus => Some(Expr.Negate(expr))
      case Operator.Bang  => Some(Expr.Not(expr))
      case _              => None

  def primary(tokens: List[Token]): ExprTokens =
    tokens match
      case Literal.Number(l) :: rest  => (Right(Expr.Literal(l.toDouble)), rest)
      case Literal.Str(l) :: rest     => (Right(Expr.Literal(l)), rest)
      case Keyword.True :: rest       => (Right(Expr.Literal(true)), rest)
      case Keyword.False :: rest      => (Right(Expr.Literal(false)), rest)
      case Keyword.Nil :: rest        => (Right(Expr.Literal(null)), rest)
      case Operator.LeftParen :: rest => parenBody(rest)
      case _                          => (Left(Error.ExpectExpression), tokens)

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody(tokens: List[Token]): ExprTokens =
    expression(tokens) match
      case (Left(e), rest) => (Left(e), rest)
      case (Right(expr), rest) =>
        rest match
          case Operator.RightParen :: remaining => (Right(Expr.Grouping(expr)), remaining)
          case _                                => (Left(Error.ExpectClosing), rest)
