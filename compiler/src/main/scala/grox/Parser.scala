package grox

object Parser:

  enum Err(msg: String):
    // case Unexpected(t: Token) extends Error(s"Unexpected '${t.lexeme}'")
    case ExpectExpression extends Err("Expect expression")
    case ExpectClosing extends Err("Expect ')' after expression")

  type Error = (Err, List[Token])
  type Success = (Expr, List[Token])
  type P = Either[Error, Success]

  // type ExprTokens = (Either[Error, Expr], List[Token])
  type BinaryOp = Token => Option[(Expr, Expr) => Expr]
  type UnaryOp = Token => Option[Expr => Expr]

  // Parse a single expression and return remaining tokens
  def parse(ts: List[Token]): P = expression(ts)

  def expression(tokens: List[Token]): P = equality(tokens)

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)*
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").
  def binary(
    op: BinaryOp,
    descendant: List[Token] => P,
  )(
    tokens: List[Token]
  ): P =
    def matchOp(ts: List[Token], l: Expr): P =
      ts match
        case token :: rest =>
          op(token) match
            case Some(fn) => descendant(rest).flatMap((r, rmn) => matchOp(rmn, fn(l, r)))
            case None     => Right(l, ts)
        case _ => Right(l, ts)

    descendant(tokens).flatMap((expr, rest) => matchOp(rest, expr))

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

  def unary(tokens: List[Token]): P =
    tokens match
      case token :: rest =>
        unaryOp(token) match
          case Some(fn) => unary(rest).flatMap((expr, rmn) => Right(fn(expr), rmn))
          case None     => primary(tokens)
      case _ => primary(tokens)

  def primary(tokens: List[Token]): P =
    tokens match
      case Literal.Number(l) :: rest  => Right(Expr.Literal(l.toDouble), rest)
      case Literal.Str(l) :: rest     => Right(Expr.Literal(l), rest)
      case Keyword.True :: rest       => Right(Expr.Literal(true), rest)
      case Keyword.False :: rest      => Right(Expr.Literal(false), rest)
      case Keyword.Nil :: rest        => Right(Expr.Literal(null), rest)
      case Operator.LeftParen :: rest => parenBody(rest)
      case _                          => Left((Err.ExpectExpression, tokens))

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody(
    tokens: List[Token]
  ): P = expression(tokens).flatMap((expr, rest) =>
    rest match
      case Operator.RightParen :: rmn => Right(Expr.Grouping(expr), rmn)
      case _                          => Left((Err.ExpectClosing, rest))
  )
