package grox

enum Stmt[A]:
  case Block(stmts: List[Stmt[A]])
  case Expression(expr: Expr)
  case Function(name: Token[A], params: Token[?], body: List[Stmt[A]])
  case If(cond: Expr, thenStmt: Stmt[A], elseStmt: Option[Stmt[A]])
  case Print(expr: Expr)
  case Return(keyword: Token[A], value: Expr)
  case Var(name: Token.Identifier[A], init: Option[Expr])
  case While(cond: Expr, body: Stmt[A])

object Stmt {}
