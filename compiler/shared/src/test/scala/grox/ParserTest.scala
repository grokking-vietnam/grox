package grox

import TokenParser.*
import Token.*
import Span.*

class ParserTest extends munit.FunSuite:

  val identifierX: Identifier[Span] = Identifier("x", empty)
  val avar: Identifier[Span] = Identifier("a", empty)

  val exprX = Expr.Literal(empty, "x")

  val num1 = Number("1", empty)
  val num2 = Number("2", empty)
  val num3 = Number("3", empty)
  val num42 = Number("42", empty)

  val expr1 = Expr.Literal(empty, 1)
  val expr2 = Expr.Literal(empty, 2)
  val expr3 = Expr.Literal(empty, 3)
  val expr42 = Expr.Literal(empty, 42)
  val span = Span(Location(0, 0, 0), Location(0, 5, 5))

  val parse = run(Parser.program)

  test("Parse empty token list"):
    val ts = List()
    assertEquals(parse(ts).isLeft, true)

  test("Parse single print Statement"):
    val ts = List(Print(empty), num1, Semicolon(empty))
    val expected = List(Stmt.Print(expr1))
    assertEquals(parse(ts), Right(expected))

  test("Val declaration"):
    val ts = List(
      Var(empty),
      avar,
      Equal(empty),
      num42,
      Semicolon(empty),
    )

    val expected = List(Stmt.Var(avar, Some(expr42)))
    assertEquals(parse(ts), Right(expected))

  test("declaration failed when missing identifier"):
    val ts = List(Var(empty))
    // val expected = Error(Message.empty)
    assertEquals(parse(ts).isLeft, true)

  test("Parse variable declaration without initializer"):
    val ts = List(Var(empty), Identifier("x", empty), Semicolon(empty))
    val expected = List(Stmt.Var(Identifier("x", empty), None))
    assertEquals(parse(ts), Right(expected))

  test("Parse full variable declaration with trailing expressions"):
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
    val expected = List(
      Stmt.Var(grox.Token.Identifier("x", empty), Some(Expr.Literal(empty, 1.0))),
      Stmt.Expression(Expr.Multiply(empty, expr2, expr3)),
    )
    assertEquals(parse(ts), Right(expected))

  test("assignment statement"):
    val ts = List(avar, Equal(empty), num1, Semicolon(empty))
    val expected = List(Stmt.Assign("a", expr1))
    assertEquals(parse(ts), Right(expected))

  test("block statement"):
    val ts = List(
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
    val expected = List(
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
      )
    )
    assertEquals(parse(ts), Right(expected))

  test("While statement with block"):
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

    val expected = List(
      Stmt.While(
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
    )

    assertEquals(parse(ts), Right(expected))

  test("While statement with a single statement"):
    // while (true) print 1;
    val ts = List(
      While(empty),
      LeftParen(empty),
      True(empty),
      RightParen(empty),

      // val a = 1
      Print(empty),
      num1,
      Semicolon(empty),
    )

    val expected = List(
      Stmt.While(
        Expr.Literal(empty, true),
        Stmt.Print(expr1),
      )
    )

    assertEquals(parse(ts), Right(expected))

  test("For loop statement"):
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

    val expected = List(Stmt.Block(List(varStmt, whileStmts)))
    // assertEquals(parse(ts), Right(expected))
    assertEquals(parse(ts), Right(expected))
