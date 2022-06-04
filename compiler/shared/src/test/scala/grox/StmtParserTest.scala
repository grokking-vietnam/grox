package grox

import grox.Parser.*
import grox.Token.*
import munit.Clue.generate

class StmtParserTest extends munit.FunSuite:

  trait TestSets:
    val identifierX = Identifier("x", ())
    val identifierXSpan = Identifier("x", span)
    val exprX = Expr.Literal("x")

    val num1 = Number("1", ())
    val num2 = Number("2", ())
    val num3 = Number("3", ())

    val expr1 = Expr.Literal(1)
    val expr2 = Expr.Literal(2)
    val expr3 = Expr.Literal(3)
    val span = Span(Location(0, 0, 0), Location(0, 5, 5))

  test("Parse full variable declaration") {
    new TestSets:
      val ts = List(
        Var(()),
        identifierX,
        Equal(()),
        num1,
        Semicolon(()),
      )
      val want = Stmt.Var(grox.Token.Identifier("x", ()), Some(Expr.Literal(1)))
      assertEquals(
        parseStmt(Inspector(List.empty, List.empty, ts)),
        Inspector(List.empty, List(want), List.empty),
      )
  }

  test("Parse variable declaration failed when missing identifier") {
    new TestSets:
      val ts = List(
        Var(())
      )
      val want = Error.ExpectVarIdentifier[Unit](ts)
      assertEquals(
        parseStmt(Inspector(List.empty, List.empty, ts)),
        Inspector(List(want), List.empty, List.empty),
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
    val ts = List(Var(()), Identifier("x", ()), Semicolon(()))
    val want = Stmt.Var(Identifier("x", ()), None)
    assertEquals(
      parseStmt(Inspector(List.empty, List.empty, ts)),
      Inspector(List.empty, List(want), List.empty),
    )
  }

  test("Parse full variable declaration with trailing expressions") {
    new TestSets:
      val ts = List(
        Var(()),
        grox.Token.Identifier("x", ()),
        Equal(()),
        num1,
        Semicolon(()),
        num2,
        Star(()),
        num3,
        Semicolon(()),
      )
      val want = List(
        Stmt.Var[Unit](grox.Token.Identifier("x", ()), Some(Expr.Literal(1.0))),
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
      val ts = List(Print(()), num1, Semicolon(()))
      val want = List(Stmt.Print[Unit](expr1))
      assertEquals(
        parseStmt(Inspector(List.empty, List.empty, ts)),
        Inspector(List.empty, want, List.empty),
      )
  }

  test("Test consume Var") {
    new TestSets:
      val ts = List(Var(()), identifierX, Semicolon(()))
      val want = Right((Var(()), ts.tail))
      assertEquals(
        consume[Unit, Var[Unit]](ts),
        want,
      )
  }

  test("Test consume failed Var") {
    new TestSets:
      val ts = List(Print(()), identifierX, Semicolon(()))
      val want = Left(Error.UnexpectedToken(ts))
      assertEquals(
        consume[Unit, Var[Unit]](ts),
        want,
      )
  }

  test("Test consume Var with Span") {
    new TestSets:
      val ts = List(Var(span), identifierXSpan, Semicolon(span))
      val want = Right((Var(span), ts.tail))
      assertEquals(
        consume[Span, Var[Span]](ts),
        want,
      )
  }

  test("Test consume Var failed with Span") {
    new TestSets:
      val ts = List(Print(span), identifierXSpan, Semicolon(span))
      val want = Left(Error.UnexpectedToken(ts))
      assertEquals(
        consume[Span, Var[Span]](ts),
        want,
      )
  }

  test("Test consume Equal") {
    new TestSets:
      val ts = List(Equal(()), identifierX, Semicolon(()))
      val want = Right((Equal(()), ts.tail))
      assertEquals(
        consume[Unit, Equal[Unit]](ts),
        want,
      )
  }

  test("Test consume failed Equal") {
    new TestSets:
      val ts = List(Print(()), identifierX, Semicolon(()))
      val want = Left(Error.UnexpectedToken(ts))
      assertEquals(
        consume[Unit, Equal[Unit]](ts),
        want,
      )
  }

  test("Test consume Equal with Span") {
    new TestSets:
      val ts = List(Equal(span), identifierXSpan, Semicolon(span))
      val want = Right((Equal(span), ts.tail))
      assertEquals(
        consume[Span, Equal[Span]](ts),
        want,
      )
  }

  test("Test consume Equal failed with Span") {
    new TestSets:
      val ts = List(Print(span), identifierXSpan, Semicolon(span))
      val want = Left(Error.UnexpectedToken(ts))
      assertEquals(
        consume[Span, Var[Span]](ts),
        want,
      )
  }

  test("Test consume Semicolon") {
    new TestSets:
      val ts = List(Semicolon(()), identifierX, Semicolon(()))
      val want = Right((Semicolon(()), ts.tail))
      assertEquals(
        consume[Unit, Semicolon[Unit]](ts),
        want,
      )
  }

  test("Test consume failed Semicolon") {
    new TestSets:
      val ts = List(Print(()), identifierX, Semicolon(()))
      val want = Left(Error.UnexpectedToken(ts))
      assertEquals(
        consume[Unit, Semicolon[Unit]](ts),
        want,
      )
  }

  test("Test consume Semicolon with Span") {
    new TestSets:
      val ts = List(Semicolon(span), identifierXSpan, Semicolon(span))
      val want = Right((Semicolon(span), ts.tail))
      assertEquals(
        consume[Span, Semicolon[Span]](ts),
        want,
      )
  }

  test("Test consume Semicolon failed with Span") {
    new TestSets:
      val ts = List(Print(span), identifierXSpan, Semicolon(span))
      val want = Left(Error.UnexpectedToken(ts))
      assertEquals(
        consume[Span, Semicolon[Span]](ts),
        want,
      )
  }

  test("Test consume LeftParen") {
    new TestSets:
      val ts = List(LeftParen(()), identifierX, LeftParen(()))
      val want = Right((LeftParen(()), ts.tail))
      assertEquals(
        consume[Unit, LeftParen[Unit]](ts),
        want,
      )
  }

  test("Test consume failed LeftParen") {
    new TestSets:
      val ts = List(Print(()), identifierX, Semicolon(()))
      val want = Left(Error.UnexpectedToken(ts))
      assertEquals(
        consume[Unit, LeftParen[Unit]](ts),
        want,
      )
  }

  test("Test consume LeftParen with Span") {
    new TestSets:
      val ts = List(LeftParen(span), identifierXSpan, Semicolon(span))
      val want = Right((LeftParen(span), ts.tail))
      assertEquals(
        consume[Span, LeftParen[Span]](ts),
        want,
      )
  }

  test("Test consume LeftParen failed with Span") {
    new TestSets:
      val ts = List(Print(span), identifierXSpan, Semicolon(span))
      val want = Left(Error.UnexpectedToken(ts))
      assertEquals(
        consume[Span, LeftParen[Span]](ts),
        want,
      )
  }

  test("Parse single print Statement") {
    new TestSets:
      val ts = List(num1, Semicolon(()))
      val want = Right(Stmt.Print[Unit](expr1), List())
      assertEquals(
        printStmt(ts),
        want,
      )
  }
