package grox

import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen, Prop}

import Parser.*
import ExprGen.*
import Token.*

class ParserTest extends munit.FunSuite:

  trait TestSets:
    val num1 = Number("1", ())
    val num2 = Number("2", ())
    val num3 = Number("3", ())
    val num4 = Number("4", ())
    val num5 = Number("5", ())
    val num6 = Number("6", ())
    val num42 = Number("42", ())

    val expr1 = Expr.Literal(1)
    val expr2 = Expr.Literal(2)
    val expr3 = Expr.Literal(3)
    val expr4 = Expr.Literal(4)
    val expr5 = Expr.Literal(5)
    val expr6 = Expr.Literal(6)
    val expr42 = Expr.Literal(42)

    val avar: Identifier[Unit] = Identifier("a", ())

  test("empty") {
    assertEquals(parse[Unit](Nil), Left(Error.ExpectExpression(Nil)))
  }

  test("primary number") {
    val ts = List(Number("42", ()))
    val want = Expr.Literal(42)
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("primary string") {
    val ts = List(Str("you rox!", ()))
    val want = Expr.Literal("you rox!")
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("primary true") {
    val ts = List(True(()))
    val want = Expr.Literal(true)
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("primary true") {
    val ts = List(False(()))
    val want = Expr.Literal(false)
    assertEquals(parse(ts), Right(want, List()))
  }

  test("primary nil") {
    val ts = List(Null(()))
    val want = Expr.Literal(null)
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("unary negate") {
    val ts = List(Minus(()), Number("42", ()))
    val want = Expr.Negate(Expr.Literal(42))
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("unary not") {
    val ts = List(Bang(()), False(()))
    val want = Expr.Not(Expr.Literal(false))
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("unary multiple minus") {
    val ts = List(Minus(()), Minus(()), Minus(()), Number("1", (())))
    val want = Expr.Negate(Expr.Negate(Expr.Negate(Expr.Literal(1))))
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("factor 2 numbers") {
    new TestSets:
      val ts = List(num2, Star(()), num5)
      val want = Expr.Multiply(expr2, expr5)
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("factor 4 numbers") {
    // 2 * 5 * 1 / 42
    new TestSets:
      val ts = List(num2, Star(()), num5, Star(()), num1, Slash(()), num42)
      val want = Expr.Divide(Expr.Multiply(Expr.Multiply(expr2, expr5), expr1), expr42)
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("factor with unary") {
    // -1 * 2 * -3
    new TestSets:
      val ts = List(Minus(()), num1, Star(()), num2, Star(()), Minus(()), num3)
      val want = Expr.Multiply(
        Expr.Multiply(Expr.Negate(expr1), expr2),
        Expr.Negate(expr3),
      )
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("term 2 numbers") {
    new TestSets:
      val ts = List(num2, Minus(()), Minus(()), num3)
      val want = Expr.Subtract(expr2, Expr.Negate(expr3))
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("term 4 numbers") {
    // 1 * -2 * 3 - 4
    new TestSets:
      val ts = List(
        num1,
        Plus(()),
        Minus(()),
        num2,
        Star(()),
        num3,
        Minus(()),
        num4,
      )
      val want = Expr.Subtract(
        Expr.Add(
          expr1,
          Expr.Multiply(Expr.Negate(expr2), expr3),
        ),
        expr4,
      )
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("comparison") {
    // 1 * 2 > -3 * 4
    new TestSets:
      val ts = List(
        num1,
        Star(()),
        num2,
        Greater(()),
        Minus(()),
        num3,
        Plus(()),
        num4,
      )
      val want = Expr.Greater(
        Expr.Multiply(expr1, expr2),
        Expr.Add(Expr.Negate(expr3), expr4),
      )
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("equality") {
    // false != 1 * 2 > -3 * 4
    new TestSets:
      val ts = List(
        False(()),
        BangEqual(()),
        num1,
        Star(()),
        num2,
        Greater(()),
        Minus(()),
        num3,
        Plus(()),
        num4,
      )
      val want = Expr.NotEqual(
        Expr.Literal(false),
        Expr.Greater(
          Expr.Multiply(expr1, expr2),
          Expr.Add(Expr.Negate(expr3), expr4),
        ),
      )
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("combination") {
    // false == !(1 * 2 > -3 + 4 / 5)
    new TestSets:
      val ts = List(
        False(()),
        EqualEqual(()),
        Bang(()),
        LeftParen(()),
        num1,
        Star(()),
        num2,
        Greater(()),
        Minus(()),
        num3,
        Plus(()),
        num4,
        Slash(()),
        num5,
        RightParen(()),
        num1,
        EqualEqual(()),
        num2,
      )
      val want = Expr.Equal(
        Expr.Literal(false),
        Expr.Not(
          Expr.Grouping(
            Expr.Greater(
              Expr.Multiply(expr1, expr2),
              Expr.Add(
                Expr.Negate(expr3),
                Expr.Divide(expr4, expr5),
              ),
            )
          )
        ),
      )
      val rmn = List(num1, EqualEqual(()), num2)

      assertEquals(parse(ts), Right(want, rmn))
  }

  test("AND OR logic") {
    new TestSets:
      // True OR (3 >= 4) AND (5 < 6)
      val ts = List(
        True(()),
        Or(()),
        LeftParen(()),
        num3,
        GreaterEqual(()),
        num4,
        RightParen(()),
        And(()),
        LeftParen(()),
        num5,
        Less(()),
        num6,
        RightParen(()),
      )
      val want = Expr.Or(
        left = Expr.Literal(true),
        right = Expr.And(
          left = Expr.Grouping(
            Expr.GreaterEqual(
              Expr.Literal(3),
              Expr.Literal(4),
            )
          ),
          right = Expr.Grouping(
            Expr.Less(
              Expr.Literal(5),
              Expr.Literal(6),
            )
          ),
        ),
      )
      val rmn = List.empty[Token[Unit]]

      assertEquals(parse(ts), Right(want, rmn))

  }

  test("error: expect expression") {
    // 1 + 2 / (3 - )
    new TestSets:
      val ts = List(
        num1,
        Plus(()),
        num2,
        Slash(()),
        LeftParen(()),
        num3,
        Minus(()),
        RightParen(()),
      )
      assertEquals(parse(ts), Left(Error.ExpectExpression(List(RightParen(())))))
  }

  test("error: expect closing paren") {
    // 1 + 2 / (3 - 4  true false
    new TestSets:
      val ts = List(
        num1,
        Plus(()),
        num2,
        Slash(()),
        LeftParen(()),
        num3,
        Minus(()),
        num4,
        True(()),
        False(()),
      )
      assertEquals(parse(ts), Left(Error.ExpectClosing(List(True(()), False(())))))
  }

  test("synchronize: until statement") {
    // a = a + 1) { print a; }
    new TestSets:
      val ts = List(
        avar,
        Equal(()),
        avar,
        Plus(()),
        avar,
        RightParen(()),
        LeftBrace(()),
        Print(()),
        avar,
        Semicolon(()),
        RightBrace(()),
      )
      val remaining = ts.dropWhile(_ != Print(()))
      assertEquals(synchronize(ts), remaining)
  }

  test("synchronize: until new expression") {
    // + 1; 2 * 3;
    new TestSets:
      val ts = List(
        Plus(()),
        num1,
        Semicolon(()),
        num2,
        Star(()),
        num3,
        Semicolon(()),
      )
      val remaining = ts.dropWhile(_ != num2)
      assertEquals(synchronize(ts), remaining)
  }

  test("assignments: expect assignment") {
    new TestSets:
      val ts = List(
        Var(()),
        avar,
        Equal(()),
        num42,
        Semicolon(()),
      )

      val expectedStmt: Stmt[Unit] = Stmt.Var(
        avar,
        Some(Expr.Literal(42)),
      )

      val inspector: Inspector[Unit] = Inspector(
        List.empty[Error[Unit]],
        List.empty[Stmt[Unit]],
        tokens = ts,
      )
      val expectedInspector = inspector.copy(
        stmts = List(expectedStmt),
        tokens = List.empty[Token[Unit]],
      )

      assertEquals(parseStmt(inspector), expectedInspector)
  }

  test("While: statement ") {
    new TestSets:
      val ts = List(
        While(()),
        LeftParen(()),
        True(()),
        RightParen(()),
        LeftBrace(()),

        // val a = 42
        Var(()),
        avar,
        Equal(()),
        num42,
        Semicolon(()),
        // a = a + a
        avar,
        Equal(()),
        avar,
        Plus(()),
        avar,
        Semicolon(()),
        RightBrace(()),
      )

      val expectedStmt: Stmt[Unit] = Stmt.While(
        Expr.Literal(true),
        Stmt.Block(
          List(
            Stmt.Var(
              avar,
              Some(Expr.Literal(42)),
            ),
            Stmt.Expression(
              Expr.Assign(
                Identifier("a", ()),
                Expr.Add(
                  Expr.Variable(
                    Identifier("a", ())
                  ),
                  Expr.Variable(
                    Identifier("a", ())
                  ),
                ),
              )
            ),
          )
        ),
      )

      val inspector: Inspector[Unit] = Inspector(
        List.empty[Error[Unit]],
        List.empty[Stmt[Unit]],
        tokens = ts,
      )
      val expectedInspector = inspector.copy(
        stmts = List(expectedStmt),
        tokens = List.empty[Token[Unit]],
      )

      assertEquals(parseStmt(inspector), expectedInspector)

  }

end ParserTest

class ParserCheck extends ScalaCheckSuite:
  property("parse numerics succesfully") {
    Prop.forAll(numericGen) { expr =>
      parse(expr.flatten) match
        case Left(_)  => false
        case Right(_) => true
    }
  }

  property("parse logicals succesfully") {
    Prop.forAll(logicalGen) { expr =>
      parse(expr.flatten) match
        case Left(_)  => false
        case Right(_) => true
    }
  }

  // TODO: property: value of original expression and that of parsed expression are equal.
end ParserCheck
