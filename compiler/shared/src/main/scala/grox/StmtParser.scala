package grox

import cats.*
import cats.syntax.all.*

import kantan.parsers.Parser as P

trait StmtParser[F[_]]:
  def parse(tokens: List[Token[Span]]): F[List[Stmt]]
  def parseExpr(tokens: List[Token[Span]]): F[Expr]

object StmtParser:

  import Token.*
  import TokenParser.{given, *}
  import ExprParser.expr

  def instance[F[_]: MonadThrow]: StmtParser[F] = new:
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

  lazy val blockStmt = stmt.rep.between(blockStart, blockEnd).map(Stmt.Block(_))
  val exprStmt = (expr <* semicolon).map(Stmt.Expression(_))
  val printStmt = (print *> expr <* semicolon).map(Stmt.Print(_))

  val varStmt = `var` *> identifier ~ (equal *> expr).? <* semicolon map:
    case (name, init) => Stmt.Var(name, init)

  lazy val ifStmt = (`if` *> expr.between(blockStart, blockEnd)
    ~ stmt ~ (`else` *> stmt).? <* semicolon).map:
    case ((cond, thenBranch), elseBranch) => Stmt.If(cond, thenBranch, elseBranch)

  lazy val assignStmt = identifier ~ equal ~ expr <* semicolon map:
    case ((name, _), value) => Stmt.Assign(name.lexeme, value)

  lazy val forStmt =
    (`for` *> ((varStmt | exprStmt) ~ (expr.? <* semicolon) ~ (expr.? <* semicolon))
      .between(blockStart, blockEnd) ~ stmt).map:
      case (((init, cond), incr), body) =>
        val whileBody = Stmt.Block(List(body) ++ incr.map(Stmt.Expression(_)))
        val whileCond = cond.getOrElse(Expr.Literal(Span.empty, true))
        Stmt.Block(List(init, Stmt.While(whileCond, whileBody)))

  lazy val whileStmt = `while` *> ExprParser.expr.between(blockStart, blockEnd)
    ~ stmt <* semicolon map:
    case (cond, body) => Stmt.While(cond, body)

  lazy val stmt: Parser[Stmt] = P.oneOf(
    exprStmt,
    blockStmt,
    ifStmt,
    printStmt,
    assignStmt,
    forStmt,
    whileStmt,
  )

  lazy val declareation = varStmt | stmt
  lazy val program = declareation.rep <* P.end
