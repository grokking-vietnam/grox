package grox

import cats.*
import cats.effect.kernel.syntax.resource
import cats.implicits.*
import grox.Parser.ExprParser
import cats.instances.*

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
    case InvalidAssignmentTartget(token: Token)
      extends Error("Invalid assignment target.", List(token))

  type ExprParser = Either[Error, (Expr, List[Token])]
  type StmtParser = Either[Error, (Stmt, List[Token])]

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
            parseStmt(inspector.copy(tokens = rest, stmts = inspector.stmts.appended(stmt)))
          case Left(err) =>
            parseStmt(
              inspector.copy(
                tokens = synchronize(inspector.tokens),
                errors = err :: inspector.errors,
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
  ): StmtParser = consume(Keyword.Var, tokens).flatMap((varToken, tokensAfterVar) =>
    tokensAfterVar
      .headOption
      .collectFirst { case token: Literal.Identifier =>
        for {
          initializer <-
            consume(Operator.Equal, tokensAfterVar.tail) match {
              case Left(_) => Right((None, tokensAfterVar.tail))
              case Right((equalToken, afterEqual)) =>
                expression(afterEqual).map { case (value, afterValue) =>
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
          case Operator.LeftBrace => blockStmt(tokens.tail)
          case Keyword.If         => ifStmt(tokens.tail)
          case Keyword.For        => forStmt(tokens.tail)
          case Keyword.Return     => returnStmt(token, tokens.tail)
          case Keyword.While      => whileStmt(tokens.tail)
          case _                  => expressionStmt(tokens)
        }
      case _ => expressionStmt(tokens)
    }

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
              declaration(ts) match {
                case Right((dclr, rest)) => block(rest, dclr :: stmts)
                case left @ Left(_) => left.asInstanceOf[Left[Error, (List[Stmt], List[Token])]]
              }
          }
        case _ => Left(Error.ExpectRightBrace(ts.tail))
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

  def forStmt(tokens: List[Token]): StmtParser =
    for {
      (leftParen, afterLeftParenTokens) <- consume(Operator.LeftParen, tokens)

      (initializerStmtOption, afterInitializerTokens): (Option[Stmt], List[Token]) <-
        afterLeftParenTokens.headOption match {

          case Some(Operator.Semicolon) => (None, afterLeftParenTokens.tail).asRight
          case Some(Keyword.Var) =>
            declaration(afterLeftParenTokens).map((declareStmt, tokens) =>
              (declareStmt.some, tokens)
            )
          case _ =>
            expressionStmt(afterLeftParenTokens).map((declareStmt, tokens) =>
              (declareStmt.some, tokens)
            )

        }

      (conditionalExprOption, afterConditionStmtTokens): (Option[Expr], List[Token]) <-
        afterInitializerTokens.headOption match {
          case Some(Operator.Semicolon) => (None, afterInitializerTokens).asRight
          case _ =>
            expression(afterLeftParenTokens).map((declareStmt, tokens) =>
              (declareStmt.some, tokens)
            )
        }

      (_, afterConditionStmtAndSemiColonTokens) <- consume(
        Operator.Semicolon,
        afterConditionStmtTokens,
      )

      (incrementExprOption, afterIncrementStmtTokens): (Option[Expr], List[Token]) <-
        afterConditionStmtAndSemiColonTokens.headOption match {
          case Some(Operator.RightParen) =>
            (None, afterConditionStmtAndSemiColonTokens.tail).asRight
          case _ =>
            expression(afterConditionStmtAndSemiColonTokens).map((declareStmt, tokens) =>
              (declareStmt.some, tokens)
            )
        }

      (_, afterIncrementStmtAndRightParenTokens) <- consume(
        Operator.RightParen,
        afterIncrementStmtTokens,
      )
      (body, afterBodyTokens) <- statement(afterLeftParenTokens)
      desugarIncrement =
        (
          if (incrementExprOption.isDefined)
            new Stmt.Block(List(body, new Stmt.Expression(incrementExprOption.get)))
          else
            body
        )
      desugarCondition =
        new Stmt.While(
          conditionalExprOption.getOrElse(new Expr.Literal(true)),
          desugarIncrement,
        )

      desugarInitializer = initializerStmtOption
        .map(initializer => new Stmt.Block(List(initializer, desugarCondition)))
        .getOrElse(desugarCondition)

    } yield (desugarInitializer, afterBodyTokens)

  def whileStmt(tokens: List[Token]): StmtParser =
    for {
      (leftParen, afterLeftParenTokens) <- consume(Operator.LeftParen, tokens)
      (conditionExpr, afterExpressionTokens) <- expression(afterLeftParenTokens)
      (rightParen, afterRightParenTokens) <- consume(Operator.RightParen, afterExpressionTokens)
      (stmt, afterStatementTokens) <- statement(afterRightParenTokens)
      (semicolon, afterSemicolonTokens) <- consume(Operator.Semicolon, afterStatementTokens)
    } yield (Stmt.While(conditionExpr, stmt), afterSemicolonTokens)

  def consume(expect: Token, tokens: List[Token]): Either[Error, (Token, List[Token])] =
    tokens.headOption match {
      case Some(expect) => Right(expect, tokens.tail)
      case _ =>
        expect match {
          case Operator.Semicolon => Left(Error.ExpectSemicolon(tokens.tail))
        }
    }

  def expression: List[Token] => ExprParser = assignment

  def or: List[Token] => ExprParser = binary(orOp, and)

  def and: List[Token] => ExprParser = binary(andOp, equality)

  def assignment(tokens: List[Token]): ExprParser =
    or(tokens) >>= ((expr: Expr, restTokens: List[Token]) =>
      restTokens.headOption match {
        case Some(equalToken @ Operator.Equal) =>
          expr match {
            case Expr.Variable(name) =>
              assignment(restTokens.tail) >>= ((value, tokens) =>
                Right((Expr.Assign(name, value), tokens)),
              )
            case _ => Left(Error.InvalidAssignmentTartget(equalToken))
          }

        case _ => Right((expr, restTokens))
      },
    )

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
            case Some(fn) =>
              descendant(rest).flatMap((r: Expr, rmn: List[Token]) => matchOp(rmn, fn(l, r)))
            case None => Right(l, ts)
        case _ => Right(l, ts)

    descendant(tokens).flatMap((expr, rest) => matchOp(rest, expr))

  val orOp: BinaryOp =
    case Keyword.Or => Some(Expr.Or.apply)
    case _          => None

  val andOp: BinaryOp =
    case Keyword.And => Some(Expr.And.apply)
    case _           => None

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

  def equality: List[Token] => ExprParser = binary(equalityOp, comparison)
  def comparison: List[Token] => ExprParser = binary(comparisonOp, term)
  def term: List[Token] => ExprParser = binary(termOp, factor)
  def factor: List[Token] => ExprParser = binary(factorOp, unary)

  def unary(tokens: List[Token]): ExprParser =
    tokens match
      case token :: rest =>
        unaryOp(token) match
          case Some(fn) => unary(rest).flatMap((expr, rmn) => Right(fn(expr), rmn))
          case None     => primary(tokens)
      case _ => primary(tokens)

  def primary(tokens: List[Token]): ExprParser =
    tokens match
      case Literal.Number(l) :: rest        => Right(Expr.Literal(l.toDouble), rest)
      case Literal.Str(l) :: rest           => Right(Expr.Literal(l), rest)
      case Keyword.True :: rest             => Right(Expr.Literal(true), rest)
      case Keyword.False :: rest            => Right(Expr.Literal(false), rest)
      case Keyword.Nil :: rest              => Right(Expr.Literal(null), rest)
      case Literal.Identifier(name) :: rest => Right(Expr.Variable(Literal.Identifier(name)), rest)
      case Operator.LeftParen :: rest       => parenBody(rest)
      case _                                => Left(Error.ExpectExpression(tokens))

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
    tokens match
      case t :: rest =>
        t match
          case Operator.Semicolon => rest
          case Keyword.Class | Keyword.Fun | Keyword.Var | Keyword.For | Keyword.If |
              Keyword.While | Keyword.Print | Keyword.Return =>
            tokens
          case _ => synchronize(rest)
      case Nil => Nil
