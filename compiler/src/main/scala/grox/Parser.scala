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
    case ExpectSemicolon(tokens: List[Token[T]]) extends Error("Expect ';' after statement", tokens)
    case ExpectRightBrace(tokens: List[Token[T]]) extends Error("Expect '}' after statement", tokens)
    case ExpectVarIdentifier(tokens: List[Token[T]])
      extends Error("Expect var identifier after 'var' declaration", tokens)
    case ExpectVar(tokens: List[Token[T]]) extends Error("Expect 'var' declaration", tokens)
    case ExpectEqual(tokens: List[Token[T]]) extends Error("Expect '=' token", tokens)
    case ExpectRightParen(tokens: List[Token[T]]) extends Error("Expect ')' token", tokens)
    case ExpectLeftParen(tokens: List[Token[T]]) extends Error("Expect '(' token", tokens)
    override def toString: String = msg

  type ExprParser[A] = Either[Error[A], (Expr, List[Token[A]])]
  type StmtParser = Either[Error[A], (Stmt, List[Token[A]])]


  type BinaryOp[A] = Token[A] => Option[(Expr, Expr) => Expr]
  type UnaryOp[A] = Token[A] => Option[Expr => Expr]

  case class Inspector[A](errors: List[Error[A]], stmts: List[Stmt], tokens: List[Token[A]])

  // Parse a single expression and return remaining tokens
  def parse[A](ts: List[Token[A]]): ExprParser[A] = expression[A](ts)

  def parseStmt[A](inspector: Inspector[A]): Inspector[A] =
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

  def expression[A](tokens: List[Token[A]]): ExprParser[A] = equality(tokens)

  def statement[A](tokens: List[Token[A]]): StmtParser[A] =
    tokens.headOption match {
      case Some(token) =>
        token match {
          case Keyword.Print      => printStmt[A](tokens.tail)
          case Operator.LeftParen => blockStmt[A](tokens.tail)
          case Keyword.If         => ifStmt[A](tokens.tail)
          case Keyword.For        => forStmt[A](tokens.tail)
          case Keyword.Return     => returnStmt[A](token, tokens.tail)
          case Keyword.While      => whileStmt[A](tokens.tail)
          case _                  => expressionStmt[A](tokens)
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
    tokens match {
      case `expect` :: tail => Right(expect, tokens.tail)
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
