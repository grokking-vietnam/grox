package grox

import scala.annotation.tailrec
import scala.reflect.{ClassTag, TypeTest}
import scala.util.control.NoStackTrace

import cats.*
import cats.implicits.*
import cats.instances.*

import grox.Parser.ExprParser

trait Parser[F[_]]:
  def parse(tokens: List[Token[Span]]): F[Expr]

object Parser:

  import Token.*

  def instance[F[_]: MonadThrow]: Parser[F] =
    new Parser[F]:

      def parse(
        tokens: List[Token[Span]]
      ): F[Expr] = Parser.parse(tokens).map { case (exp, _) => exp }.liftTo[F]

  enum Error(msg: String, tokens: List[Token[Span]]) extends NoStackTrace:
    case ExpectExpression(tokens: List[Token[Span]]) extends Error("Expect expression", tokens)
    case ExpectClosing(tokens: List[Token[Span]])
      extends Error("Expect ')' after expression", tokens)
    case ExpectSemicolon(tokens: List[Token[Span]])
      extends Error("Expect ';' after statement", tokens)
    case ExpectRightBrace(tokens: List[Token[Span]])
      extends Error("Expect '}' after statement", tokens)
    case ExpectVarIdentifier(tokens: List[Token[Span]])
      extends Error("Expect var identifier after 'var' declaration", tokens)
    case InvalidAssignmentTarget(token: Token[Span])
      extends Error("Invalid assignment target.", List(token))
    case UnexpectedToken(tokens: List[Token[Span]]) extends Error("Unexpected token error", tokens)
    case MaxNumberOfArgumentsExceeded(tokens: List[Token[Span]]) extends Error("Max number of arguments exceeded", tokens)

  type ExprParser = Either[Error, (Expr, List[Token[Span]])]
  type StmtParser = Either[Error, (Stmt, List[Token[Span]])]
  type ArgsParser = Either[Error, (List[Expr], List[Token[Span]])]

  type BinaryOp = Token[Span] => Option[Expr => Expr => Expr]
  type UnaryOp = Token[Span] => Option[Expr => Expr]

  case class Inspector(errors: List[Error], stmts: List[Stmt], tokens: List[Token[Span]])
  object Inspector:
    def apply(): Inspector = Inspector(Nil, Nil, Nil)

  // Parse a single expression and return remaining tokens
  def parse(ts: List[Token[Span]]): ExprParser = expression(ts)

  @tailrec
  def parseStmt(inspector: Inspector): Inspector =
    inspector.tokens match
      case Nil => inspector
      case _ =>
        declaration(inspector.tokens) match
          case Right(stmt, rest) =>
            val updatedInspector = inspector.copy(tokens = rest, stmts = inspector.stmts :+ stmt)
            parseStmt(updatedInspector)
          case Left(err) =>
            parseStmt(
              inspector.copy(
                tokens = synchronize(inspector.tokens.tail),
                errors = inspector.errors :+ err,
              )
            )

  def declaration(tokens: List[Token[Span]]): StmtParser =
    tokens.headOption match
      case Some(Class(_)) => ???
      case Some(Fun(_))   => ???
      case Some(Var(_))   => varDeclaration(tokens)
      case _              => statement(tokens)

  def varDeclaration(
    tokens: List[Token[Span]]
  ): StmtParser = consume[Var[Span]](tokens).flatMap((varToken, tokensAfterVar) =>
    tokensAfterVar
      .headOption
      .collectFirst { case token: Identifier[Span] =>
        for {
          initializer <-
            consume[Equal[Span]](tokensAfterVar.tail) match {
              case Left(_) => Right((None, tokensAfterVar.tail))
              case Right(equalToken, afterEqual) =>
                expression(afterEqual).map { case (value, afterValue) =>
                  (Option(value), afterValue)
                }
            }
          (maybeInitializer, afterInitializer) = initializer
          semicolonCnsm <- consume[Semicolon[Span]](afterInitializer)
        } yield (Stmt.Var(token, maybeInitializer), semicolonCnsm._2)
      }
      .getOrElse(Left(Error.ExpectVarIdentifier(tokens)))
  )

  def expression(tokens: List[Token[Span]]): ExprParser = or(tokens)

  def or: List[Token[Span]] => ExprParser = binary(orOp, and)

  def and: List[Token[Span]] => ExprParser = binary(andOp, equality)

  def assignmentExpr(
    tokens: List[Token[Span]]
  ): StmtParser =
    for {
      (identifer, restTokens) <- consume[Identifier[Span]](tokens)
      iden <-
        identifer match
          case a: Identifier[Span] => Right(a)
          case _                   => Left(Error.UnexpectedToken(tokens))
      (equalToken, afterEqualToken) <- consume[Equal[Span]](restTokens)
      (valueExpr, afterValue) <- expression(afterEqualToken)
    } yield (Stmt.Assign(iden.lexeme, valueExpr), afterValue)

  def assignment(
    tokens: List[Token[Span]]
  ): StmtParser =

    val attemptToParseAssignmentExpr =
      for {
        (assingExpr, afterValue) <- assignmentExpr(tokens)
        semicolonCnsm <- consume[Semicolon[Span]](afterValue)

      } yield (assingExpr, semicolonCnsm._2)

    attemptToParseAssignmentExpr.recoverWith { case error => expressionStmt(tokens) }

  def statement(tokens: List[Token[Span]]): StmtParser =
    tokens.headOption match
      case Some(token) =>
        token match
          case Print(_)     => printStmt(tokens.tail)
          case LeftBrace(_) => blockStmt(tokens.tail)
          case If(_)        => ifStmt(tokens.tail)
          case For(_)       => forStmt(tokens.tail)
          case Return(_)    => returnStmt(token, tokens.tail)
          case While(_)     => whileStmt(tokens.tail)
          case _            => assignment(tokens)
      case _ => assignment(tokens)

  def expressionStmt(tokens: List[Token[Span]]): StmtParser =
    for {
      pr <- expression(tokens)
      cnsm <- consume[Semicolon[Span]](pr._2)
    } yield (Stmt.Expression(pr._1), cnsm._2)

  def printStmt(tokens: List[Token[Span]]): StmtParser =
    for {
      pr <- expression(tokens)
      cnsm <- consume[Semicolon[Span]](pr._2)
    } yield (Stmt.Print(pr._1), cnsm._2)

  def consume[TokenType <: Token[Span]: ClassTag](
    tokens: List[Token[Span]]
  ): Either[Error, (Token[Span], List[Token[Span]])] =
    tokens match
      case (head: TokenType) :: _ => Right(head, tokens.tail)
      case _                      => Left(Error.UnexpectedToken(tokens))

  def blockStmt(tokens: List[Token[Span]]): StmtParser =
    def block(
      ts: List[Token[Span]],
      stmts: List[Stmt] = List.empty[Stmt],
    ): Either[Error, (List[Stmt], List[Token[Span]])] =
      ts.headOption match
        case Some(token) =>
          token match
            case RightBrace(_) => Right(stmts, ts.tail)
            case _ =>
              declaration(ts) match
                case Right(dclr, rest) => block(rest, stmts :+ dclr)
                case left @ Left(_) =>
                  left.asInstanceOf[Left[Error, (List[Stmt], List[Token[Span]])]]
        case _ => Left(Error.ExpectRightBrace(ts))

    block(tokens).map((stmts, rest) => (Stmt.Block(stmts), rest))

  def ifStmt(tokens: List[Token[Span]]): StmtParser =
    for {
      leftParenCnsm <- consume[LeftParen[Span]](tokens)
      afterLeftParen <- expression(leftParenCnsm._2)
      (condition, afterCond) = afterLeftParen
      rightParenCnsm <- consume[RightParen[Span]](afterCond)
      afterRightParen <- statement(rightParenCnsm._2)
      (thenBranch, afterThen) = afterRightParen
      maybeElseBranch <- afterThen
        .headOption
        .collectFirst { case Else(_) =>
          statement(afterThen.tail).map { case (stmt, rest) => (Option(stmt), rest) }
        }
        .getOrElse(Right(None, afterThen))
      (elseBranch, afterElse) = maybeElseBranch
    } yield (Stmt.If(condition, thenBranch, elseBranch), afterElse)

  def returnStmt(keyword: Token[Span], tokens: List[Token[Span]]): StmtParser = ???

  def forStmt(tokens: List[Token[Span]]): StmtParser =
    for {

      (leftParen, afterLeftParenTokens) <- consume[LeftParen[Span]](tokens)

      (initializerStmtOption, afterInitializerTokens): (Option[Stmt], List[Token[Span]]) <-
        afterLeftParenTokens.headOption match

          case Some(_: Semicolon[Span]) => (None, afterLeftParenTokens.tail).asRight
          case Some(_: Var[Span]) =>
            declaration(afterLeftParenTokens).map((declareStmt, toks) => (declareStmt.some, toks))
          case _ =>
            assignment(afterLeftParenTokens).map((declareStmt, toks) => (declareStmt.some, toks))

      (conditionalExprOption, afterConditionStmtTokens): (Option[Expr], List[Token[Span]]) <-
        afterInitializerTokens.headOption match
          case Some(_: Semicolon[Span]) =>
            (None, afterInitializerTokens).asRight // todo afterInitializerTokens.tail?
          case _ =>
            expression(afterInitializerTokens).map((declareStmt, tokens) =>
              (declareStmt.some, tokens)
            )

      (_, afterConditionStmtAndSemiColonTokens) <- consume[Semicolon[Span]](
        afterConditionStmtTokens
      )

      (incrementExprOption, afterIncrementStmtTokens): (Option[Stmt], List[Token[Span]]) <-
        afterConditionStmtAndSemiColonTokens.headOption match
          case Some(_: RightParen[Span]) =>
            (None, afterConditionStmtAndSemiColonTokens.tail).asRight
          case _ =>
            assignmentExpr(afterConditionStmtAndSemiColonTokens).map((declareStmt, tokens) =>
              (declareStmt.some, tokens)
            )

      (_, afterIncrementStmtAndRightParenTokens) <- consume[RightParen[Span]](
        afterIncrementStmtTokens
      )

      (body, afterBodyTokens) <- statement(afterIncrementStmtAndRightParenTokens)

      desugarIncrement =
        if (incrementExprOption.isDefined)
          Stmt.Block(List(body, incrementExprOption.get))
        else
          body
      desugarCondition = Stmt.While(
        conditionalExprOption.getOrElse(Expr.Literal(Span.empty, true)),
        desugarIncrement,
      )

      desugarInitializer = initializerStmtOption
        .map(initializer => Stmt.Block(List(initializer, desugarCondition)))
        .getOrElse(desugarCondition)

    } yield (desugarInitializer, afterBodyTokens)

  def whileStmt(tokens: List[Token[Span]]): StmtParser =
    for {
      (leftParen, afterLeftParenTokens) <- consume[LeftParen[Span]](tokens)
      (conditionExpr, afterExpressionTokens) <- expression(afterLeftParenTokens)
      (rightParen, afterRightParenTokens) <- consume[RightParen[Span]](afterExpressionTokens)
      (stmt, afterStatementTokens) <- statement(afterRightParenTokens)
    } yield (Stmt.While(conditionExpr, stmt), afterStatementTokens)

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)  *
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").
  def binary(
    op: BinaryOp,
    descendant: List[Token[Span]] => ExprParser,
  )(
    tokens: List[Token[Span]]
  ): ExprParser =
    def matchOp(ts: List[Token[Span]], l: Expr): ExprParser =
      ts match
        case token :: rest =>
          op(token) match
            case Some(fn) => descendant(rest).flatMap((r, rmn) => matchOp(rmn, fn(l)(r)))
            case None     => Right(l, ts)
        case _ => Right(l, ts)

    descendant(tokens).flatMap((expr, rest) => matchOp(rest, expr))

  def orOp: BinaryOp =
    case Or(t) => Some(Expr.Or.apply.curried(t))
    case _     => None

  def andOp: BinaryOp =
    case And(t) => Some(Expr.And.apply.curried(t))
    case _      => None

  def equalityOp: BinaryOp =
    case EqualEqual(t) => Some(Expr.Equal.apply.curried(t))
    case BangEqual(t)  => Some(Expr.NotEqual.apply.curried(t))
    case _             => None

  def comparisonOp: BinaryOp =
    case Less(t)         => Some(Expr.Less.apply.curried(t))
    case LessEqual(t)    => Some(Expr.LessEqual.apply.curried(t))
    case Greater(t)      => Some(Expr.Greater.apply.curried(t))
    case GreaterEqual(t) => Some(Expr.GreaterEqual.apply.curried(t))
    case _               => None

  def termOp: BinaryOp =
    case Plus(t)  => Some(Expr.Add.apply.curried(t))
    case Minus(t) => Some(Expr.Subtract.apply.curried(t))
    case _        => None

  def factorOp: BinaryOp =
    case Star(t)  => Some(Expr.Multiply.apply.curried(t))
    case Slash(t) => Some(Expr.Divide.apply.curried(t))
    case _        => None

  def unaryOp: UnaryOp =
    case Minus(t) => Some(Expr.Negate.apply.curried(t))
    case Bang(t)  => Some(Expr.Not.apply.curried(t))
    case _        => None

  def equality = binary(equalityOp, comparison)
  def comparison = binary(comparisonOp, term)
  def term = binary(termOp, factor)
  def factor = binary(factorOp, unary)

  def unary(tokens: List[Token[Span]]): ExprParser =
    tokens match
      case token :: rest =>
        unaryOp(token) match
          case Some(fn) => unary(rest).flatMap((expr, rmn) => Right(fn(expr), rmn))
          case None     => call(tokens)
      case _ => call(tokens)

  def call(tokens: List[Token[Span]]): ExprParser =
    primary(tokens).flatMap((expr, rest) => handleCall(expr, rest))

  def primary(tokens: List[Token[Span]]): ExprParser =
    tokens match
      case Number(l, tag) :: rest        => Right(Expr.Literal(tag, l.toDouble), rest)
      case Str(l, tag) :: rest           => Right(Expr.Literal(tag, l), rest)
      case True(tag) :: rest             => Right(Expr.Literal(tag, true), rest)
      case False(tag) :: rest            => Right(Expr.Literal(tag, false), rest)
      case Null(tag) :: rest             => Right(Expr.Literal(tag, ()), rest)
      case Identifier(name, tag) :: rest => Right(Expr.Variable(tag, name), rest)
      case LeftParen(tag) :: rest        => parenBody(rest)
      case _                             => Left(Error.ExpectExpression(tokens))

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody(
    tokens: List[Token[Span]]
  ): ExprParser = expression(tokens).flatMap((expr, rest) =>
    rest match
      case RightParen(_) :: rmn => Right(Expr.Grouping(expr), rmn)
      case _                    => Left(Error.ExpectClosing(rest))
  )

  // Discard tokens until a new expression/statement is found
  def synchronize(tokens: List[Token[Span]]): List[Token[Span]] =
    tokens match
      case t :: rest =>
        t match
          case Semicolon(_) => rest
          case Class(_) | Fun(_) | Var(_) | For(_) | If(_) | While(_) | Print(_) | Return(_) =>
            tokens
          case _ => synchronize(rest)
      case List() => List()


  private def handleCall(expr: Expr, tokens: List[Token[Span]]): ExprParser =
    tokens.headOption match {
      case Some(LeftParen(tag)) => finishCall(expr, tokens.tail) match {
        case Right((expr_, tokens_)) => handleCall(expr_, tokens_)
        case l => l
      }
      case _ => Right(expr -> tokens)
    }

  private def finishCall(callee: Expr, tokens: List[Token[Span]]): ExprParser = tokens.headOption match {
      case Some(token@RightParen(tag)) => Right(Expr.Call(Span.empty, callee, token, List.empty[Expr]), tokens)
      case _ => parseArgs(List.empty[Expr], tokens).flatMap((args, rest) =>
        if (args.length > MAXIMUM_ARGUMENTS) {
          Left(Error.MaxNumberOfArgumentsExceeded(tokens))
        } else rest.headOption.collectFirst {
          case close@RightParen(_) => Right(close)
        }.getOrElse(Left(Error.UnexpectedToken(rest))).map{rightParen => Expr.Call(Span.empty, callee, rightParen, args)}
      )
    }

  private def parseArgs(args: List[Expr], tokens: List[Token[Span]]): ArgsParser =
    expression(tokens).flatMap((expr, rest) => rest match
        case Comma(_) :: rmn => parseArgs(args :+ expr, rmn)
        case RightParen(_) :: rmn => Right(args :+ expr, rest)
        case _ => Left(Error.UnexpectedToken(rest))
    )