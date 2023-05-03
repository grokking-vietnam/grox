package grox

import grox.Parser.*
import grox.Token.*

import Span.*

class StmtParserTest extends munit.FunSuite:

  val identifierX: Identifier[Span] = Identifier("x", empty)
  val avar: Identifier[Span] = Identifier("a", empty)

  val exprX = Expr.Literal(empty, "x")

  val num1 = Number("1", empty)
  val num2 = Number("2", empty)
  val num3 = Number("3", empty)

  val expr1 = Expr.Literal(empty, 1)
  val expr2 = Expr.Literal(empty, 2)
  val expr3 = Expr.Literal(empty, 3)
  val span = Span(Location(0, 0, 0), Location(0, 5, 5))

  test("Parse full variable declaration") {
    val ts = List(
      Var(empty),
      identifierX,
      Equal(empty),
      num1,
      Semicolon(empty),
    )
    val want = Stmt.Var(identifierX, Some(expr1))
    assertEquals(
      _parseStmt(Inspector(List.empty, List.empty, ts)),
      Inspector(List.empty, List(want), List.empty),
    )
  }

  test("Parse variable declaration failed when missing identifier") {
    val ts = List(Var(empty))
    val want = Error.ExpectVarIdentifier(ts)
    assertEquals(
      _parseStmt(Inspector(List.empty, List.empty, ts)),
      Inspector(List(want), List.empty, List.empty),
    )
  }

  test("Parse empty token list") {
    val ts = List()
    assertEquals(
      _parseStmt(Inspector(List.empty, List.empty, ts)),
      Inspector(List.empty, List.empty, List.empty),
    )
  }

  test("Parse variable declaration without initializer") {
    val ts = List(Var(empty), Identifier("x", empty), Semicolon(empty))
    val want = Stmt.Var(Identifier("x", empty), None)
    assertEquals(
      _parseStmt(Inspector(List.empty, List.empty, ts)),
      Inspector(List.empty, List(want), List.empty),
    )
  }

  test("Parse full variable declaration with trailing expressions") {
    val ts = List(
      Var(empty),
      grox.Token.Identifier("x", empty),
      Equal(empty),
      num1,
      Semicolon(empty),
      num2,
      Star(empty),
      num3,
      Semicolon(empty),
    )
    val want = List(
      Stmt.Var(grox.Token.Identifier("x", empty), Some(Expr.Literal(empty, 1.0))),
      Stmt.Expression(Expr.Multiply(empty, expr2, expr3)),
    )
    assertEquals(
      _parseStmt(Inspector(List.empty, List.empty, ts)),
      Inspector(
        List.empty,
        want,
        List.empty,
      ),
    )
  }

  test("Parse single print Statement") {
    val ts = List(Print(empty), num1, Semicolon(empty))
    val want = List(Stmt.Print(expr1))
    assertEquals(
      _parseStmt(Inspector(List.empty, List.empty, ts)),
      Inspector(List.empty, want, List.empty),
    )
  }

  test("Parse print stmt should return err when missing closing ')'") {
    val tokensMissingRightParent = List(
      Print(Span(Location(0, 0, 0), Location(0, 5, 5))),
      LeftParen(Span(Location(0, 5, 5), Location(0, 6, 6))),
      Str("Hello world", Span(Location(0, 6, 6), Location(0, 19, 19))))

    //    current return Right(List())
    parseStmt(tokensMissingRightParent) match
      case Left(e) =>
        assert(e.getMessage.equals("Expect ')' after expression"))
  }

  test("Parse print stmt should return err when wrong closing '}'") {
    val tokensMissingRightParent = List(
      Print(Span(Location(0, 0, 0), Location(0, 5, 5))),
      LeftParen(Span(Location(0, 5, 5), Location(0, 6, 6))),
      Str("Hello world", Span(Location(0, 6, 6), Location(0, 19, 19))),
      RightBrace(Span(Location(0,19,19), Location(0,20,20))))

    //    current return Right(List())
    parseStmt(tokensMissingRightParent) match
      case Left(e) =>
        assert(e.getMessage.equals("Expect ')' after expression"))
  }

  test("Test consume Var") {
    val ts = List(Var(span), identifierX, Semicolon(span))
    val want = Right((Var(span), ts.tail))
    assertEquals(
      consume[Var[Span]](ts),
      want,
    )
  }

  test("Test consume Var failed") {
    val ts = List(Print(span), identifierX, Semicolon(span))
    val want = Left(Error.UnexpectedToken(ts))
    assertEquals(
      consume[Var[Span]](ts),
      want,
    )
  }

  test("Parse single print Statement") {
    val ts = List(num1, Semicolon(empty))
    val want = Right(Stmt.Print(expr1), List())
    assertEquals(
      printStmt(ts),
      want,
    )
  }

  test("assignment statement") {
    val ts = List(avar, Equal(empty), num1, Semicolon(empty))
    val want = Stmt.Assign("a", expr1)
    assertEquals(
      assignment(ts),
      Right(want, Nil),
    )
  }

  test("Val declaration") {
    val ts = List(
      Var(empty),
      avar,
      Equal(empty),
      num1,
      Semicolon(empty),
    )

    val expectedStmt: Stmt = Stmt.Var(
      avar,
      Some(Expr.Literal(empty, 1.0)),
    )

    val inspector: Inspector = Inspector(
      List.empty[Error],
      List.empty[Stmt],
      tokens = ts,
    )
    val expectedInspector = inspector.copy(
      stmts = List(expectedStmt),
      tokens = Nil,
    )

    assertEquals(_parseStmt(inspector), expectedInspector)
  }

  test("While: statement ") {
    // while (true) { var a = 1;  a = a + a; }
    val ts = List(
      While(empty),
      LeftParen(empty),
      True(empty),
      RightParen(empty),
      LeftBrace(empty),

      // val a = 1
      Var(empty),
      avar,
      Equal(empty),
      num1,
      Semicolon(empty),
      // a = a + a
      avar,
      Equal(empty),
      avar,
      Plus(empty),
      avar,
      Semicolon(empty),
      RightBrace(empty),
    )

    val expectedStmt: Stmt = Stmt.While(
      Expr.Literal(empty, true),
      Stmt.Block(
        List(
          Stmt.Var(
            avar,
            Some(Expr.Literal(empty, 1)),
          ),
          Stmt.Assign(
            "a",
            Expr.Add(
              empty,
              Expr.Variable(empty, "a"),
              Expr.Variable(empty, "a"),
            ),
          ),
        )
      ),
    )

    val inspector = Inspector().copy(tokens = ts)

    val expectedInspector = Inspector().copy(
      stmts = List(expectedStmt)
    )

    assertEquals(_parseStmt(inspector), expectedInspector)

  }

  test("For loop statement") {
    // for (var i = 0; i < 10; i = i + 1) print i;
    val ivar: Identifier[Span] = Identifier("i", empty)

    val ts = List(
      For(empty),

      // (var i = 0; i < 10; i = i + 1)
      LeftParen(empty),
      Var(empty),
      ivar,
      Equal(empty),
      Number("0", empty),
      Semicolon(empty),
      ivar,
      Less(empty),
      Number("10", empty),
      Semicolon(empty),
      ivar,
      Equal(empty),
      ivar,
      Plus(empty),
      num1,
      RightParen(empty),

      // print i;
      Print(empty),
      ivar,
      Semicolon(empty),
    )

    val varStmt: Stmt = Stmt.Var(
      ivar,
      Some(Expr.Literal(empty, 0)),
    )

    val whileStmts: Stmt = Stmt.While(
      Expr.Less(
        empty,
        Expr.Variable(empty, "i"),
        Expr.Literal(empty, 10),
      ),
      Stmt.Block(
        List(
          Stmt.Print(Expr.Variable(empty, "i")),
          Stmt.Assign(
            "i",
            Expr.Add(
              empty,
              Expr.Variable(empty, "i"),
              Expr.Literal(empty, 1),
            ),
          ),
        )
      ),
    )

    val expectedStmts = Stmt.Block(List(varStmt, whileStmts))

    val inspector = Inspector().copy(tokens = ts)

    val expectedInspector = Inspector().copy(
      stmts = List(expectedStmts)
    )

    assertEquals(_parseStmt(inspector), expectedInspector)
  }
