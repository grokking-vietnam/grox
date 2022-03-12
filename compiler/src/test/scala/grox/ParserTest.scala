package grox

import Parser.*

class ParserTest extends munit.FunSuite:

  trait TestSets:
    val num1 = Literal.Number("1")
    val num2 = Literal.Number("2")
    val num3 = Literal.Number("3")
    val num4 = Literal.Number("4")
    val num5 = Literal.Number("5")
    val num42 = Literal.Number("42")

    val expr1 = Expr.Literal(1)
    val expr2 = Expr.Literal(2)
    val expr3 = Expr.Literal(3)
    val expr4 = Expr.Literal(4)
    val expr5 = Expr.Literal(5)
    val expr42 = Expr.Literal(42)

  test("empty") {
    assertEquals(parse(Nil), Left(Error.ExpectExpression(Nil)))
  }

  test("primary number") {
    val ts = List(Literal.Number("42"))
    val want = Expr.Literal(42)
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("primary string") {
    val ts = List(Literal.Str("you rox!"))
    val want = Expr.Literal("you rox!")
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("primary true") {
    val ts = List(Keyword.True)
    val want = Expr.Literal(true)
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("primary true") {
    val ts = List(Keyword.False)
    val want = Expr.Literal(false)
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("primary nil") {
    val ts = List(Keyword.Nil)
    val want = Expr.Literal(null)
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("unary negate") {
    val ts = List(Operator.Minus, Literal.Number("42"))
    val want = Expr.Negate(Expr.Literal(42))
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("unary not") {
    val ts = List(Operator.Bang, Keyword.False)
    val want = Expr.Not(Expr.Literal(false))
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("unary multiple minus") {
    val ts = List(Operator.Minus, Operator.Minus, Operator.Minus, Literal.Number("1"))
    val want = Expr.Negate(Expr.Negate(Expr.Negate(Expr.Literal(1))))
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("factor 2 numbers") {
    new TestSets:
      val ts = List(num2, Operator.Star, num5)
      val want = Expr.Multiply(expr2, expr5)
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("factor 4 numbers") {
    // 2 * 5 * 1 / 42
    new TestSets:
      val ts = List(num2, Operator.Star, num5, Operator.Star, num1, Operator.Slash, num42)
      val want = Expr.Divide(Expr.Multiply(Expr.Multiply(expr2, expr5), expr1), expr42)
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("factor with unary") {
    // -1 * 2 * -3
    new TestSets:
      val ts = List(Operator.Minus, num1, Operator.Star, num2, Operator.Star, Operator.Minus, num3)
      val want = Expr.Multiply(
        Expr.Multiply(Expr.Negate(expr1), expr2),
        Expr.Negate(expr3),
      )
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("term 2 numbers") {
    new TestSets:
      val ts = List(num2, Operator.Minus, Operator.Minus, num3)
      val want = Expr.Subtract(expr2, Expr.Negate(expr3))
      assertEquals(parse(ts), Right(want, Nil))
  }

  test("term 4 numbers") {
    // 1 * -2 * 3 - 4
    new TestSets:
      val ts = List(
        num1,
        Operator.Plus,
        Operator.Minus,
        num2,
        Operator.Star,
        num3,
        Operator.Minus,
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
        Operator.Star,
        num2,
        Operator.Greater,
        Operator.Minus,
        num3,
        Operator.Plus,
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
        Keyword.False,
        Operator.BangEqual,
        num1,
        Operator.Star,
        num2,
        Operator.Greater,
        Operator.Minus,
        num3,
        Operator.Plus,
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
        Keyword.False,
        Operator.EqualEqual,
        Operator.Bang,
        Operator.LeftParen,
        num1,
        Operator.Star,
        num2,
        Operator.Greater,
        Operator.Minus,
        num3,
        Operator.Plus,
        num4,
        Operator.Slash,
        num5,
        Operator.RightParen,
        num1,
        Operator.EqualEqual,
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
      val rmn = List(num1, Operator.EqualEqual, num2)

      assertEquals(parse(ts), Right(want, rmn))
  }

  test("error: expect expression") {
    // 1 + 2 / (3 - )
    new TestSets:
      val ts = List(
        num1,
        Operator.Plus,
        num2,
        Operator.Slash,
        Operator.LeftParen,
        num3,
        Operator.Minus,
        Operator.RightParen,
      )
      assertEquals(parse(ts), Left(Error.ExpectExpression(List(Operator.RightParen))))
  }

  test("error: expect closing paren") {
    // 1 + 2 / (3 - 4  true false
    new TestSets:
      val ts = List(
        num1,
        Operator.Plus,
        num2,
        Operator.Slash,
        Operator.LeftParen,
        num3,
        Operator.Minus,
        num4,
        Keyword.True,
        Keyword.False,
      )
      assertEquals(parse(ts), Left(Error.ExpectClosing(List(Keyword.True, Keyword.False))))
  }

end ParserTest
