package grox

import scala.annotation.tailrec
import scala.reflect.{ClassTag, TypeTest}
import scala.util.control.NoStackTrace

import cats.*
import cats.effect.kernel.syntax.resource
import cats.implicits.*
import grox.Parser.ExprParser
import cats.instances.*

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
    case ExpectSemicolon(tokens: List[Token[T]]) extends Error("Expect ';' after statement", tokens)
    case ExpectRightBrace(tokens: List[Token[T]])
      extends Error("Expect '}' after statement", tokens)
    case ExpectVarIdentifier(tokens: List[Token[T]])
      extends Error("Expect var identifier after 'var' declaration", tokens)
    case InvalidAssignmentTarget(token: Token[T])
      extends Error("Invalid assignment target.", List(token))
    case UnexpectedToken(tokens: List[Token[T]]) extends Error("Unexpected token error", tokens)

  type ExprParser[A] = Either[Error[A], (Expr, List[Token[A]])]
  type StmtParser[A] = Either[Error[A], (Stmt[A], List[Token[A]])]

  type BinaryOp[A] = Token[A] => Option[(Expr, Expr) => Expr]
  type UnaryOp[A] = Token[A] => Option[Expr => Expr]

  case class Inspector[A](errors: List[Error[A]], stmts: List[Stmt[A]], tokens: List[Token[A]])

  // Parse a single expression and return remaining tokens
  def parse[A](ts: List[Token[A]]): ExprParser[A] = expression[A](ts)

  @tailrec
  def parseStmt[A](inspector: Inspector[A]): Inspector[A] =
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

  def declaration[A](tokens: List[Token[A]]): StmtParser[A] =
    tokens.headOption match
      case Some(Class(_)) => ???
      case Some(Fun(_))   => ???
      case Some(Var(_))   => varDeclaration(tokens)
      case _              => statement(tokens)

  def varDeclaration[A](
    tokens: List[Token[A]]
  ): StmtParser[A] = consume[A, Var[A]](tokens).flatMap((varToken, tokensAfterVar) =>
    tokensAfterVar
      .headOption
      .collectFirst { case token: Identifier[A] =>
        for {
          initializer <-
            consume[A, Equal[A]](tokensAfterVar.tail) match {
              case Left(_) => Right((None, tokensAfterVar.tail))
              case Right(equalToken, afterEqual) =>
                expression(afterEqual).map { case (value, afterValue) =>
                  (Option(value), afterValue)
                }
            }
          (maybeInitializer, afterInitializer) = initializer
          semicolonCnsm <- consume[A, Semicolon[A]](afterInitializer)
        } yield (Stmt.Var(token, maybeInitializer), semicolonCnsm._2)
      }
      .getOrElse(Left(Error.ExpectVarIdentifier(tokens)))
  )

  def expression[A](tokens: List[Token[A]]): ExprParser[A] = assignment(tokens)

  def or[A]: List[Token[A]] => ExprParser[A] = binary(orOp, and)

  def and[A]: List[Token[A]] => ExprParser[A] = binary(andOp, equality)

  def assignment[A](
    tokens: List[Token[A]]
  ): ExprParser[A] = or(tokens).flatMap((expr: Expr, restTokens: List[Token[A]]) =>
    restTokens.headOption match {
      case Some(equalToken @ Equal(_)) =>
        expr match {
          case Expr.Variable(name) =>
            assignment(restTokens.tail).flatMap((value, tokens) =>
              Right((Expr.Assign(name, value), tokens)),
            )
          case _ => Left(Error.InvalidAssignmentTarget(equalToken))
        }

      case _ => Right((expr, restTokens))
    },
  )

  def statement[A](tokens: List[Token[A]]): StmtParser[A] =
    tokens.headOption match
      case Some(token) =>
        token match
          case Print(_)     => printStmt[A](tokens.tail)
          case LeftBrace(_) => blockStmt[A](tokens.tail)
          case If(_)        => ifStmt[A](tokens.tail)
          case For(_)       => forStmt[A](tokens.tail)
          case Return(_)    => returnStmt[A](token, tokens.tail)
          case While(_)     => whileStmt[A](tokens.tail)
          case _            => expressionStmt[A](tokens)
      case _ => expressionStmt(tokens)

  def expressionStmt[A](tokens: List[Token[A]]): StmtParser[A] =
    for {
      pr <- expression(tokens)
      cnsm <- consume[A, Semicolon[A]](pr._2)
    } yield (Stmt.Expression(pr._1), cnsm._2)

  def printStmt[A](tokens: List[Token[A]]): StmtParser[A] =
    for {
      pr <- expression(tokens)
      cnsm <- consume[A, Semicolon[A]](pr._2)
    } yield (Stmt.Print(pr._1), cnsm._2)

  def consume[A, TokenType <: Token[A]: ClassTag](
    tokens: List[Token[A]]
  ): Either[Error[A], (Token[A], List[Token[A]])] =
    tokens match
      case (head: TokenType) :: _ => Right(head, tokens.tail)
      case _                      => Left(Error.UnexpectedToken(tokens))

  def blockStmt[A](tokens: List[Token[A]]): StmtParser[A] =
    def block(
      ts: List[Token[A]],
      stmts: List[Stmt[A]] = List.empty[Stmt[A]],
    ): Either[Error[A], (List[Stmt[A]], List[Token[A]])] =
      ts.headOption match
        case Some(token) =>
          token match
            case RightBrace(_) => Right(stmts, ts.tail)
            case _ =>
              declaration(ts) match
                case Right(dclr, rest) => block(rest, stmts :+ dclr)
                case left @ Left(_) =>
                  left.asInstanceOf[Left[Error[A], (List[Stmt[A]], List[Token[A]])]]
        case _ => Left(Error.ExpectRightBrace(ts))

    block(tokens).map((stmts, rest) => (Stmt.Block(stmts), rest))

  def ifStmt[A](tokens: List[Token[A]]): StmtParser[A] =
    for {
      leftParenCnsm <- consume[A, LeftParen[A]](tokens)
      afterLeftParen <- expression(leftParenCnsm._2)
      (condition, afterCond) = afterLeftParen
      rightParenCnsm <- consume[A, RightParen[A]](afterCond)
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

  def returnStmt[A](keyword: Token[A], tokens: List[Token[A]]): StmtParser[A] = ???

  def forStmt[A](tokens: List[Token[A]]): StmtParser[A] = ???

  def whileStmt[A](tokens: List[Token[A]]): StmtParser[A] =
    for {
      (leftParen, afterLeftParenTokens) <- consume[A, LeftParen[A]](tokens)
      (conditionExpr, afterExpressionTokens) <- expression(afterLeftParenTokens)
      (rightParen, afterRightParenTokens) <- consume[A, RightParen[A]](afterExpressionTokens)
      (stmt, afterStatementTokens) <-
        val stmt = statement(afterRightParenTokens)
        println(s"stmt = $stmt")
        stmt
      // (semicolon, afterSemicolonTokens) <- consume[A, Semicolon[A]](afterStatementTokens)
    } yield (Stmt.While(conditionExpr, stmt), afterStatementTokens)

  // Parse binary expressions that share this grammar
  // ```
  //    expr   -> descendant (OPERATOR descendant)  *
  // ```
  // Consider "equality" expression as an example. Its direct descendant is "comparison"
  // and its OPERATOR is ("==" | "!=").
  def binary[A](
    op: BinaryOp[A],
    descendant: List[Token[A]] => ExprParser[A],
  )(
    tokens: List[Token[A]]
  ): ExprParser[A] =
    def matchOp(ts: List[Token[A]], l: Expr): ExprParser[A] =
      ts match
        case token :: rest =>
          op(token) match
            case Some(fn) => descendant(rest).flatMap((r, rmn) => matchOp(rmn, fn(l, r)))
            case None     => Right(l, ts)
        case _ => Right(l, ts)

    descendant(tokens).flatMap((expr, rest) => matchOp(rest, expr))

  def orOp[A]: BinaryOp[A] =
    case Or(_) => Some(Expr.Or.apply)
    case _     => None

  def andOp[A]: BinaryOp[A] =
    case And(_) => Some(Expr.And.apply)
    case _      => None

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

  def unary[A](tokens: List[Token[A]]): ExprParser[A] =
    tokens match
      case token :: rest =>
        unaryOp(token) match
          case Some(fn) => unary(rest).flatMap((expr, rmn) => Right(fn(expr), rmn))
          case None     => primary(tokens)
      case _ => primary(tokens)

  def primary[A](tokens: List[Token[A]]): ExprParser[A] =
    tokens match
      case Number(l, _) :: rest             => Right(Expr.Literal(l.toDouble), rest)
      case Str(l, _) :: rest                => Right(Expr.Literal(l), rest)
      case True(_) :: rest                  => Right(Expr.Literal(true), rest)
      case False(_) :: rest                 => Right(Expr.Literal(false), rest)
      case Null(_) :: rest                  => Right(Expr.Literal(null), rest)
      case Identifier(name, tag: A) :: rest => Right(Expr.Variable[A](Identifier(name, tag)), rest)
      case LeftParen(_) :: rest             => parenBody(rest)
      case _                                => Left(Error.ExpectExpression(tokens))

  // Parse the body within a pair of parentheses (the part after "(")
  def parenBody[A](
    tokens: List[Token[A]]
  ): ExprParser[A] = expression(tokens).flatMap((expr, rest) =>
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
