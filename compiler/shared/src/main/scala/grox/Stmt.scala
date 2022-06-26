package grox

enum Stmt[A]:
  case Block(stmts: List[Stmt[A]])
  case Expression(expr: Expr[A])
  case Function(name: Token[A], params: Token[?], body: List[Stmt[A]])
  case If(cond: Expr[A], thenStmt: Stmt[A], elseStmt: Option[Stmt[A]])
  case Print(expr: Expr[A])
  case Return(keyword: Token[A], value: Expr[A])
  case Var(name: Token.Identifier[A], init: Option[Expr[A]])
  case While(cond: Expr[A], body: Stmt[A])

object Stmt {}
