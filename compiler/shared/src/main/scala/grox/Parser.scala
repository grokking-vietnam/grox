package grox

import cats.*
import cats.syntax.all.*

import kantan.parsers.Parser as P

trait Parser[F[_]]:
  def parse(tokens: List[Token[Span]]): F[List[Stmt]]
  def parseExpr(tokens: List[Token[Span]]): F[Expr]

object Parser:

  import Token.*
  import TokenParser.{given, *}
  import ExprParser.expr

  def instance[F[_]: MonadThrow]: Parser[F] = new:
    def parseExpr(tokens: List[Token[Span]]): F[Expr] = run(expr)(tokens).liftTo[F]
    def parse(tokens: List[Token[Span]]): F[List[Stmt]] = run(program)(tokens).liftTo[F]

  val semicolon = token.collect:
    case Semicolon(tag) => tag

  val `for` = token.collect:
    case For(tag) => tag

  val `if` = token.collect:
    case If(tag) => tag

  val print = token.collect:
    case Print(tag) => tag

  val `while` = token.collect:
    case While(tag) => tag

  val `var` = token.collect:
    case Var(tag) => tag

  val `else` = token.collect:
    case Else(tag) => tag

  val blockStart = token.collect:
    case LeftBrace(tag) => tag

  val blockEnd = token.collect:
    case RightBrace(tag) => tag

  val equal = token.collect:
    case Equal(tag) => tag

  val identifier = token.collect:
    case t @ Identifier(_, _) => t

  lazy val blockStmt = declareation.rep.between(blockStart, blockEnd).map(Stmt.Block(_))
  lazy val exprStmt = (expr <* semicolon).map(Stmt.Expression(_))
  lazy val printStmt = (print *> expr <* semicolon).map(Stmt.Print(_))

  lazy val varStmt = `var` *> identifier ~ (equal *> expr).? <* semicolon map:
  case (name, init) => Stmt.Var(name, init)

  lazy val ifStmt = (`if` *> expr.between(blockStart, blockEnd)
    ~ stmt ~ (`else` *> stmt).? <* semicolon).map:
    case ((cond, thenBranch), elseBranch) => Stmt.If(cond, thenBranch, elseBranch)

  lazy val assignExpr = (identifier <* equal) ~ expr map:
  case (name, value) => Stmt.Assign(name.lexeme, value)

  lazy val assignStmt = assignExpr <* semicolon

  lazy val forStmt = (`for` *> ((varStmt | exprStmt) ~ (expr.? <* semicolon) ~ assignExpr.?)
    .between(groupStart, groupEnd) ~ stmt).map:
    case (((init, cond), incr), body) =>
      val whileBody = Stmt.Block(List(body) ++ incr)
      val whileCond = cond.getOrElse(Expr.Literal(Span.empty, true))
      Stmt.Block(List(init, Stmt.While(whileCond, whileBody)))

  lazy val whileStmt = (`while` *> expr.between(groupStart, groupEnd)) ~ stmt map:
  case (cond, body) => Stmt.While(cond, body)

  lazy val stmt: TP[Stmt] = P.oneOf(
    whileStmt,
    forStmt,
    exprStmt.backtrack,
    blockStmt,
    ifStmt,
    printStmt,
    assignStmt,
  )

  lazy val declareation = varStmt | stmt
  lazy val program = declareation.rep <* P.end
