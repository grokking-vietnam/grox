package grox

enum Stmt:
  case Block(stmts: List[Stmt])
  case Expression(expr: Expr)
  case Function(name: Token, params: Token, body: List[Stmt])
  case If(cond: Expr, stmt: Stmt, elseStmt: Stmt)
  case Print(expr: Expr)
  case Return(keyword: Token, value: Expr)
  case Var(name: Token, init: Expr)
  case While(cond: Expr, body: Stmt)

object Stmt {}
