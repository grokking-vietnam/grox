package grox

import Parser.*
import munit.Clue.generate

class StmtParserTest extends munit.FunSuite {

  trait TestSets:
    val identifierX = Literal.Identifier("x")
    val exprX = Expr.Literal("x")

    val num1 = Literal.Number("1")
    val num2 = Literal.Number("2")
    val num3 = Literal.Number("3")

    val expr1 = Expr.Literal(1)
    val expr2 = Expr.Literal(2)
    val expr3 = Expr.Literal(3)

  test("Parse full variable declaration") {
    val ts = List(
      Keyword.Var,
      Literal.Identifier("x"),
      Operator.Equal,
      Literal.Number("1"),
      Operator.Semicolon,
    )
    val want = Stmt.Var(Literal.Identifier("x"), Some(Expr.Literal(1)))
    assertEquals(
      parseStmt(Inspector(List.empty, List.empty, ts)),
      Inspector(List.empty, List(want), List.empty),
    )
  }

  test("Parse empty token list") {
    val ts = List()
    assertEquals(
      parseStmt(Inspector(List.empty, List.empty, ts)),
      Inspector(List.empty, List.empty, List.empty),
    )
  }

  test("Parse variable declaration without var identifier") {
    val ts = List(Keyword.Var, Literal.Identifier("x"), Operator.Semicolon)
    val want = Stmt.Var(Literal.Identifier("x"), None)
    assertEquals(
      parseStmt(Inspector(List.empty, List.empty, ts)),
      Inspector(List.empty, List(want), List.empty),
    )
  }

  test("Parse full variable declaration with trailing expressions") {
    new TestSets:
      val ts = List(
        Keyword.Var,
        Literal.Identifier("x"),
        Operator.Equal,
        Literal.Number("1"),
        Operator.Semicolon,
        num2,
        Operator.Star,
        num3,
        Operator.Semicolon,
      )
      val want = List(
        Stmt.Var(Literal.Identifier("x"), Some(Expr.Literal(1.0))),
        Stmt.Expression(Expr.Multiply(expr2, expr3)),
      )
      assertEquals(
        parseStmt(Inspector(List.empty, List.empty, ts)),
        Inspector(
          List.empty,
          want,
          List.empty,
        ),
      )
  }

  test("Parse single print Statement") {
    new TestSets:
      val ts = List(Keyword.Print, identifierX, Operator.Semicolon)
      val want = List(Stmt.Print(exprX))
      assertEquals(
        parseStmt(Inspector(List.empty, List.empty, ts)),
        Inspector(List.empty, want, List.empty),
      )
  }

  test("Parse single print Statement") {
    new TestSets:
      val ts = List(Keyword.Print, identifierX, Operator.Semicolon)
      val want = List(Stmt.Print(exprX))
      assertEquals(
        parseStmt(Inspector(List.empty, List.empty, ts)),
        Inspector(List.empty, want, List.empty),
      )
  }
}
