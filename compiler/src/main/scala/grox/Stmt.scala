package grox

enum Stmt:
  case Block(stmts: List[Stmt])
  case Expression(expr: Expr)
  case Function(name: Token[Unit], params: Token[Unit], body: List[Stmt])
  case If(cond: Expr, thenStmt: Stmt, elseStmt: Option[Stmt])
  case Print(expr: Expr)
  case Return(keyword: Token[Unit], value: Expr)
  case Var(name: Token.Identifier[Unit], init: Option[Expr])
  case While(cond: Expr, body: Stmt)

object Stmt {}
