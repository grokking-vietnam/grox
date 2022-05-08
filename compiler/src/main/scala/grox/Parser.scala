package grox

import cats.*

import scala.reflect.ClassTag

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
    case ExpectSemicolon(tokens: List[Token]) extends Error("Expect ';' after statement", tokens)
    case ExpectRightBrace(tokens: List[Token]) extends Error("Expect '}' after statement", tokens)
    case ExpectVarIdentifier(tokens: List[Token])
      extends Error("Expect var identifier after 'var' declaration", tokens)
    case ExpectVar(tokens: List[Token]) extends Error("Expect 'var' declaration", tokens)
    case ExpectEqual(tokens: List[Token]) extends Error("Expect '=' token", tokens)
    case ExpectRightParen(tokens: List[Token]) extends Error("Expect ')' token", tokens)
    case ExpectLeftParen(tokens: List[Token]) extends Error("Expect '(' token", tokens)

  type ExprParser = Either[Error, (Expr, List[Token])]
  type StmtParser = Either[Error, (Stmt, List[Token])]
//  type ValidationResult[A] = ValidatedNec[Error, A]

  type BinaryOp = Token => Option[(Expr, Expr) => Expr]
  type UnaryOp = Token => Option[Expr => Expr]

  case class Inspector(errors: List[Error], stmts: List[Stmt], tokens: List[Token])

  // Parse a single expression and return remaining tokens
  def parse(ts: List[Token]): ExprParser = expression(ts)

  def parseStmt(inspector: Inspector): Inspector =
    inspector.tokens match
      case Nil => inspector
      case _ =>
        declaration(inspector.tokens) match {
          case Right((stmt, rest)) =>
            parseStmt(inspector.copy(tokens = rest, stmts = inspector.stmts :+ stmt))
          case Left(err) =>
            parseStmt(
              inspector.copy(
                tokens = synchronize(inspector.tokens),
                errors = inspector.errors :+ err,
              )
            )
        }

  def declaration(tokens: List[Token]): StmtParser =
    tokens.headOption match {
      case Some(Keyword.Class) => ???
      case Some(Keyword.Fun)   => ???
      case Some(Keyword.Var)   => varDeclaration(tokens)
      case _                   => statement(tokens)
    }

  def varDeclaration(
    tokens: List[Token]
  ): StmtParser = consume(Keyword.Var, tokens).flatMap(varCnsm =>
    varCnsm
      ._2
      .headOption
      .collectFirst { case token: Literal.Identifier =>
        for {
          initializer <-
            consume(Operator.Equal, varCnsm._2.tail) match {
              case Left(_) => Right((None, varCnsm._2.tail))
              case Right(afterEqual) =>
                expression(afterEqual._2).map { case (value, afterValue) =>
                  (Option(value), afterValue)
                }
            }
          (maybeInitializer, afterInitializer) = initializer
          semicolonCnsm <- consume(Operator.Semicolon, afterInitializer)
        } yield (Stmt.Var(token, maybeInitializer), semicolonCnsm._2)
      }
      .getOrElse(Left(Error.ExpectVarIdentifier(tokens)))
  )

  def statement(tokens: List[Token]): StmtParser =
    tokens.headOption match {
      case Some(token) =>
        token match {
          case Keyword.Print      => printStmt(tokens.tail)
          case Operator.LeftParen => blockStmt(tokens.tail)
          case Keyword.If         => ifStmt(tokens.tail)
          case Keyword.For        => forStmt(tokens.tail)
          case Keyword.Return     => returnStmt(token, tokens.tail)
          case Keyword.While      => whileStmt(tokens.tail)
          case _                  => expressionStmt(tokens)
        }
      case _ => expressionStmt(tokens)
    }

  def expression(tokens: List[Token]): ExprParser = equality(tokens)

  def expressionStmt(tokens: List[Token]): StmtParser =
    for {
      pr <- expression(tokens)
      cnsm <- consume(Operator.Semicolon, pr._2)
    } yield (Stmt.Expression(pr._1), cnsm._2)

  def printStmt(tokens: List[Token]): StmtParser =
    for {
      pr <- expression(tokens)
      cnsm <- consume(Operator.Semicolon, pr._2)
    } yield (Stmt.Print(pr._1), cnsm._2)

  def blockStmt(tokens: List[Token]): StmtParser =
    def block(
      ts: List[Token],
      stmts: List[Stmt] = List.empty[Stmt],
    ): Either[Error, (List[Stmt], List[Token])] =
      ts.headOption match {
        case Some(token) =>
          token match {
            case Operator.RightBrace => Right(stmts, ts)
            case _ =>
              declaration(tokens) match {
                case Right((dclr, rest)) => block(rest, dclr :: stmts)
                case left @ Left(_) => left.asInstanceOf[Left[Error, (List[Stmt], List[Token])]]
              }
          }
        case _ => Left(Error.ExpectRightBrace(ts))
      }

    block(tokens).map((stmts, rest) => (Stmt.Block(stmts), rest))

  def ifStmt(tokens: List[Token]): StmtParser =
    for {
      leftParenCnsm <- consume(Operator.LeftParen, tokens)
      afterLeftParen <- expression(leftParenCnsm._2)
      (condition, afterCond) = afterLeftParen
      rightParenCnsm <- consume(Operator.RightParen, afterCond)
      afterRightParen <- statement(rightParenCnsm._2)
      (thenBranch, afterThen) = afterRightParen
      maybeElseBranch <- afterThen
        .headOption
        .collectFirst { case Keyword.Else =>
          statement(afterThen.tail).map { case (stmt, rest) => (Option(stmt), rest) }
        }
        .getOrElse(Right(None, afterThen))
      (elseBranch, afterElse) = maybeElseBranch
    } yield (Stmt.If(condition, thenBranch, elseBranch), afterElse)

  def returnStmt(keyword: Token, tokens: List[Token]): StmtParser = ???

  def forStmt(tokens: List[Token]): StmtParser = ???

  def whileStmt(tokens: List[Token]): StmtParser = ???

  def consume(expect: Token, tokens: List[Token]): Either[Error, (Token, List[Token])] =
    tokens.headOption match {
      case Some(head) =>
        if (head == expect) { Right(expect, tokens.tail) }
        else {
          expect match {
            case Operator.Semicolon  => Left(Error.ExpectSemicolon(tokens))
            case Keyword.Var         => Left(Error.ExpectVar(tokens))
            case Operator.Equal      => Left(Error.ExpectEqual(tokens))
            case Operator.LeftParen  => Left(Error.ExpectLeftParen(tokens))
            case Operator.RightParen => Left(Error.ExpectRightParen(tokens))
          }
        }
      case _ =>
        expect match {
          case Operator.Semicolon  => Left(Error.ExpectSemicolon(tokens))
          case Keyword.Var         => Left(Error.ExpectVar(tokens))
          case Operator.Equal      => Left(Error.ExpectEqual(tokens))
          case Operator.LeftParen  => Left(Error.ExpectLeftParen(tokens))
          case Operator.RightParen => Left(Error.ExpectRightParen(tokens))
        }
    }

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)  *
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").
  def binary(
    op: BinaryOp,
    descendant: List[Token] => ExprParser,
  )(
    tokens: List[Token]
  ): ExprParser =
    def matchOp(ts: List[Token], l: Expr): ExprParser =
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

  def unary(tokens: List[Token]): ExprParser =
    tokens match
      case token :: rest =>
        unaryOp(token) match
          case Some(fn) => unary(rest).flatMap((expr, rmn) => Right(fn(expr), rmn))
          case None     => primary(tokens)
      case _ => primary(tokens)

  def primary(tokens: List[Token]): ExprParser =
    tokens match
      case Literal.Number(l) :: rest     => Right(Expr.Literal(l.toDouble), rest)
      case Literal.Str(l) :: rest        => Right(Expr.Literal(l), rest)
      case Literal.Identifier(l) :: rest => Right(Expr.Literal(l), rest)
      case Keyword.True :: rest          => Right(Expr.Literal(true), rest)
      case Keyword.False :: rest         => Right(Expr.Literal(false), rest)
      case Keyword.Nil :: rest           => Right(Expr.Literal(null), rest)
      case Operator.LeftParen :: rest    => parenBody(rest)
      case _                             => Left(Error.ExpectExpression(tokens))

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody(
    tokens: List[Token]
  ): ExprParser = expression(tokens).flatMap((expr, rest) =>
    rest match
      case Operator.RightParen :: rmn => Right(Expr.Grouping(expr), rmn)
      case _                          => Left(Error.ExpectClosing(rest))
  )

  // Discard tokens until a new expression/statement is found
  def synchronize(tokens: List[Token]): List[Token] =
    tokens match {
      case t :: rest =>
        t match
          case Operator.Semicolon => rest
          case Keyword.Class | Keyword.Fun | Keyword.Var | Keyword.For | Keyword.If |
              Keyword.While | Keyword.Print | Keyword.Return =>
            tokens
          case _ => synchronize(rest)
      case Nil => Nil
    }
