package grox

enum Stmt:
  case Block(stmts: List[Stmt])
  case Expression(expr: Expr)
  case If(cond: Expr, thenStmt: Stmt, elseStmt: Option[Stmt])
  case Print(expr: Expr)
  case Var(name: Token.Identifier[Span], init: Option[Expr])
  case While(cond: Expr, body: Stmt)
  case Assign(name: String, value: Expr)
