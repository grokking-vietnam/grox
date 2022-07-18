package grox

enum Stmt:
  case Block(stmts: List[Stmt])
  case Expression(expr: Expr)
  case Function(name: Token[Span], params: Token[Span], body: List[Stmt])
  case If(cond: Expr, thenStmt: Stmt, elseStmt: Option[Stmt])
  case Print(expr: Expr)
  case Return(keyword: Token[Span], value: Expr)
  case Var(name: Token.Identifier[Span], init: Option[Expr])
  case While(cond: Expr, body: Stmt)

object Stmt {}
