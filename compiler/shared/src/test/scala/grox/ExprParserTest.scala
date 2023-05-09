package grox

import cats.syntax.all.*

import munit.FunSuite

import Token.*
import Span.*

class ExprParserTest extends munit.FunSuite:

  val num1 = Number("1", empty)
  val num2 = Number("2", empty)
  val num3 = Number("3", empty)
  val num4 = Number("4", empty)
  val num5 = Number("5", empty)
  val num6 = Number("6", empty)
  val num42 = Number("42", empty)

  val expr1 = Expr.Literal(empty, 1)
  val expr2 = Expr.Literal(empty, 2)
  val expr3 = Expr.Literal(empty, 3)
  val expr4 = Expr.Literal(empty, 4)
  val expr5 = Expr.Literal(empty, 5)
  val expr6 = Expr.Literal(empty, 6)
  val avar: Identifier[Span] = Identifier("a", empty)

  val expr42 = Expr.Literal(empty, 42)

  val parse = ExprParser.parse(ExprParser.expr)

  test("empty"):
    assertEquals(parse(Nil), Left(ExprParser.Error(0)))

  test("primary number"):
    val ts = List(Number("42", empty))
    assertEquals(parse(ts), Right(expr42))

  test("primary string"):
    val ts = List(Str("you rox!", empty))
    val want = Expr.Literal(empty, "you rox!")
    assertEquals(parse(ts), Right(want))

  test("primary true"):
    val ts = List(True(empty))
    val want = Expr.Literal(empty, true)
    assertEquals(parse(ts), Right(want))

  test("primary true"):
    val ts = List(False(empty))
    val want = Expr.Literal(empty, false)
    assertEquals(parse(ts), Right(want))

  test("primary nil"):
    val ts = List(Null(empty))
    val want = Expr.Literal(empty, ())
    assertEquals(parse(ts), Right(want))

  test("primary variable"):
    val avar: Identifier[Span] = Identifier("a", empty)
    val ts = List(avar)
    val want = Expr.Variable(empty, "a")
    assertEquals(parse(ts), Right(want))

  test("primary group"):
    val ts = List(
      LeftParen(empty),
      True(empty),
      RightParen(empty),
    )
    val want = Expr.Grouping(Expr.Literal(empty, true))
    assertEquals(parse(ts), Right(want))

  test("unary negate"):
    val ts = List(Minus(empty), Number("42", empty))
    val want = Expr.Negate(empty, expr42)
    assertEquals(parse(ts), Right(want))

  test("unary not"):
    val ts = List(Bang(empty), False(empty))
    val want = Expr.Not(empty, Expr.Literal(empty, false))
    assertEquals(parse(ts), Right(want))

  test("unary multiple minus"):
    val ts = List(Minus(empty), Minus(empty), Minus(empty), Number("1", empty))
    val want = Expr.Negate(empty, Expr.Negate(empty, Expr.Negate(empty, Expr.Literal(empty, 1))))
    assertEquals(parse(ts), Right(want))

  test("factor 2 numbers"):
    val ts = List(num2, Star(empty), num5)
    val want = Expr.Multiply(empty, expr2, expr5)
    assertEquals(parse(ts), Right(want))

  test("factor 4 numbers"):
    // 2 * 5 * 1 / 42
    val ts = List(num2, Star(empty), num5, Star(empty), num1, Slash(empty), num42)
    val want = Expr.Divide(
      empty,
      Expr.Multiply(empty, Expr.Multiply(empty, expr2, expr5), expr1),
      expr42,
    )
    assertEquals(parse(ts), Right(want))

  test("factor with unary"):
    // -1 * 2 * -3
    val ts = List(Minus(empty), num1, Star(empty), num2, Star(empty), Minus(empty), num3)
    val want = Expr.Multiply(
      empty,
      Expr.Multiply(empty, Expr.Negate(empty, expr1), expr2),
      Expr.Negate(empty, expr3),
    )
    assertEquals(parse(ts), Right(want))

  test("term 2 numbers"):
    val ts = List(num2, Minus(empty), Minus(empty), num3)
    val want = Expr.Subtract(empty, expr2, Expr.Negate(empty, expr3))
    assertEquals(parse(ts), Right(want))

  test("factor 4 numbers"):
    // 2 * 5 * 1 / 42
    val ts = List(num2, Star(empty), num5, Star(empty), num1, Slash(empty), num42)
    val want = Expr.Divide(
      empty,
      Expr.Multiply(empty, Expr.Multiply(empty, expr2, expr5), expr1),
      expr42,
    )
    assertEquals(parse(ts), Right(want))

  test("factor with unary"):
    // -1 * 2 * -3
    val ts = List(Minus(empty), num1, Star(empty), num2, Star(empty), Minus(empty), num3)
    val want = Expr.Multiply(
      empty,
      Expr.Multiply(empty, Expr.Negate(empty, expr1), expr2),
      Expr.Negate(empty, expr3),
    )
    assertEquals(parse(ts), Right(want))

  test("term 2 numbers"):
    val ts = List(num2, Minus(empty), Minus(empty), num3)
    val want = Expr.Subtract(empty, expr2, Expr.Negate(empty, expr3))
    assertEquals(parse(ts), Right(want))

  test("term 4 numbers"):
    // 1 * -2 * 3 - 4
    val ts = List(
      num1,
      Plus(empty),
      Minus(empty),
      num2,
      Star(empty),
      num3,
      Minus(empty),
      num4,
    )
    val want = Expr.Subtract(
      empty,
      Expr.Add(
        empty,
        expr1,
        Expr.Multiply(empty, Expr.Negate(empty, expr2), expr3),
      ),
      expr4,
    )
    assertEquals(parse(ts), Right(want))

  test("comparison"):
    // 1 * 2 > -3 * 4
    val ts = List(
      num1,
      Star(empty),
      num2,
      Greater(empty),
      Minus(empty),
      num3,
      Plus(empty),
      num4,
    )
    val want = Expr.Greater(
      empty,
      Expr.Multiply(empty, expr1, expr2),
      Expr.Add(empty, Expr.Negate(empty, expr3), expr4),
    )
    assertEquals(parse(ts), Right(want))

  test("equality"):
    // false != 1 * 2 > -3 * 4
    val ts = List(
      False(empty),
      BangEqual(empty),
      num1,
      Star(empty),
      num2,
      Greater(empty),
      Minus(empty),
      num3,
      Plus(empty),
      num4,
    )
    val want = Expr.NotEqual(
      empty,
      Expr.Literal(empty, false),
      Expr.Greater(
        empty,
        Expr.Multiply(empty, expr1, expr2),
        Expr.Add(empty, Expr.Negate(empty, expr3), expr4),
      ),
    )
    assertEquals(parse(ts), Right(want))

  test("equality"):
    // false == !true
    val ts = List(
      False(empty),
      EqualEqual(empty),
      Bang(empty),
      True(empty),
    )
    val want = Expr.Equal(
      empty,
      Expr.Literal(empty, false),
      Expr.Not(
        empty,
        Expr.Literal(empty, true),
      ),
    )
    val rmn = List(num1, EqualEqual(empty), num2)
    assertEquals(parse(ts), Right(want))

  test("grouping"):
    // false == !(true)
    val ts = List(
      False(empty),
      EqualEqual(empty),
      Bang(empty),
      LeftParen(empty),
      True(empty),
      RightParen(empty),
    )
    val want = Expr.Equal(
      empty,
      Expr.Literal(empty, false),
      Expr.Not(
        empty,
        Expr.Grouping(
          Expr.Literal(empty, true)
        ),
      ),
    )
    val rmn = List(num1, EqualEqual(empty), num2)
    println(parse(ts))
    assertEquals(parse(ts), Right(want))

  test("combination"):
    // false == !(1 * 2 > -3 + 4 / 5)
    val ts = List(
      False(empty),
      EqualEqual(empty),
      Bang(empty),
      LeftParen(empty),
      num1,
      Star(empty),
      num2,
      Greater(empty),
      Minus(empty),
      num3,
      Plus(empty),
      num4,
      Slash(empty),
      num5,
      RightParen(empty),
      // num1,
      // EqualEqual(empty),
      // num2,
    )
    val want = Expr.Equal(
      empty,
      Expr.Literal(empty, false),
      Expr.Not(
        empty,
        Expr.Grouping(
          Expr.Greater(
            empty,
            Expr.Multiply(empty, expr1, expr2),
            Expr.Add(
              empty,
              Expr.Negate(empty, expr3),
              Expr.Divide(empty, expr4, expr5),
            ),
          )
        ),
      ),
    )
    val rmn = List(num1, EqualEqual(empty), num2)

    assertEquals(parse(ts), Right(want))

  test("AND OR logic"):
    // True OR (3 >= 4) AND (5 < 6)
    val ts = List(
      True(empty),
      Or(empty),
      LeftParen(empty),
      num3,
      GreaterEqual(empty),
      num4,
      RightParen(empty),
      And(empty),
      LeftParen(empty),
      num5,
      Less(empty),
      num6,
      RightParen(empty),
    )
    val want = Expr.Or(
      empty,
      left = Expr.Literal(empty, true),
      right = Expr.And(
        empty,
        left = Expr.Grouping(
          Expr.GreaterEqual(
            empty,
            Expr.Literal(empty, 3),
            Expr.Literal(empty, 4),
          )
        ),
        right = Expr.Grouping(
          Expr.Less(
            empty,
            Expr.Literal(empty, 5),
            Expr.Literal(empty, 6),
          )
        ),
      ),
    )

    assertEquals(parse(ts), Right(want))

  test("error: expect expression"):
    // 1 + 2 / (3 - )
    val ts = List(
      num1,
      Plus(empty),
      num2,
      Slash(empty),
      LeftParen(empty),
      num3,
      Minus(empty),
      RightParen(empty),
    )
    assertEquals(parse(ts), Left(ExprParser.Error(0)))

  test("error: expect closing paren"):
    // 1 + 2 / (3 - 4  true false
    val ts = List(
      num1,
      Plus(empty),
      num2,
      Slash(empty),
      LeftParen(empty),
      num3,
      Minus(empty),
      num4,
      True(empty),
      False(empty),
    )
    assertEquals(parse(ts), Left(ExprParser.Error(0)))

  test("1 + nil"):
    val ts = List(Number("1", empty), Plus(empty), Null(empty))
    val want = Expr.Add(empty, Expr.Literal(empty, 1), Expr.Literal(empty, ()))
    assertEquals(parse(ts), Right(want))
