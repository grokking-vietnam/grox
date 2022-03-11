package grox

object Parser:

  enum Error(msg: String):
    // case Unexpected(t: Token) extends Error(s"Unexpected '${t.lexeme}'")
    case ExpectExpression extends Error("Expect expression")
    case ExpectClosing extends Error("Expect ')' after expression")

  type ExprTokens = (Either[Error, Expr], List[Token])
  type BinaryOp = Token => Option[(Expr, Expr) => Expr]
  type UnaryOp = Token => Option[Expr => Expr]

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
    op: BinaryOp,
    descendant: List[Token] => ExprTokens,
  )(
    tokens: List[Token]
  ): ExprTokens =
    def matchOp(ts: List[Token], l: Expr): ExprTokens =
      ts match
        case token :: rest =>
          op(token) match
            case Some(fn) =>
              descendant(rest) match
                case (Left(e), rest)       => (Left(e), rest)
                case (Right(r), remaining) => matchOp(remaining, fn(l, r))
            case None => (Right(l), ts)
        case _ => (Right(l), ts)

    descendant(tokens) match
      case (Left(e), rest)     => (Left(e), rest)
      case (Right(expr), rest) => matchOp(rest, expr)

  val equalityOp: BinaryOp =
    case Operator.EqualEqual => Some(Expr.Equal)
    case Operator.BangEqual  => Some(Expr.NotEqual)
    case _                   => None

  val comparisonOp: BinaryOp =
    case Operator.Less         => Some(Expr.Less)
    case Operator.LessEqual    => Some(Expr.LessEqual)
    case Operator.Greater      => Some(Expr.Greater)
    case Operator.GreaterEqual => Some(Expr.GreaterEqual)
    case _                     => None

  val termOp: BinaryOp =
    case Operator.Plus  => Some(Expr.Add)
    case Operator.Minus => Some(Expr.Subtract)
    case _              => None

  val factorOp: BinaryOp =
    case Operator.Star  => Some(Expr.Multiply)
    case Operator.Slash => Some(Expr.Divide)
    case _              => None

  val unaryOp: UnaryOp =
    case Operator.Minus => Some(Expr.Negate)
    case Operator.Bang  => Some(Expr.Not)
    case _              => None

  def equality = binary(equalityOp, comparison)
  def comparison = binary(comparisonOp, term)
  def term = binary(termOp, factor)
  def factor = binary(factorOp, unary)

  def unary(tokens: List[Token]): ExprTokens =
    tokens match
      case token :: rest =>
        unaryOp(token) match
          case Some(fn) =>
            unary(rest) match
              case (Left(e), rest)     => (Left(e), rest)
              case (Right(expr), rest) => (Right(fn(expr)), rest)
          case None => primary(tokens)
      case _ => primary(tokens)

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
