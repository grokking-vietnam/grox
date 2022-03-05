package grox

import cats.implicits._

import Expr._

class ExprShowTest extends munit.FunSuite {
  test("Show single Plus expression right") {
    val plus = Expr.Plus(Expr.Num(1.0), Expr.Num(2.0))
    assertEquals(plus.show, "1.0 + 2.0")
  }

  test("Show Plus with other expressions right") {
    val plus = Expr.Plus(
      Expr.Plus(Expr.Num(1.0), Expr.Num(2.0)),
      Expr.Minus(Expr.Num(1.0), Expr.Num(2.0)),
    )
    assertEquals(plus.show, "(1.0 + 2.0) + (1.0 - 2.0)")
  }

  test("Show single Minus expression right") {
    val minus = Expr.Minus(Expr.Num(1.0), Expr.Num(2.0))
    assertEquals(minus.show, "1.0 - 2.0")
  }

  test("Show Minus with other expressions right") {
    val minus = Expr.Minus(
      Expr.Plus(Expr.Num(1.0), Expr.Num(2.0)),
      Expr.Minus(Expr.Num(1.0), Expr.Num(2.0)),
    )
    assertEquals(minus.show, "(1.0 + 2.0) - (1.0 - 2.0)")
  }

  test("Show single Times expression right") {
    val times = Expr.Times(Expr.Num(1.0), Expr.Num(2.0))
    assertEquals(times.show, "1.0 × 2.0")
  }

  test("Show Times with other expressions right") {
    val times = Expr.Times(
      Expr.Plus(Expr.Num(1.0), Expr.Num(2.0)),
      Expr.Minus(Expr.Num(1.0), Expr.Num(2.0)),
    )
    assertEquals(times.show, "(1.0 + 2.0) × (1.0 - 2.0)")
  }

  test("Show single Divide expression right") {
    val divide = Expr.Divide(Expr.Num(1.0), Expr.Num(2.0))
    assertEquals(divide.show, "1.0 ÷ 2.0")
  }

  test("Show Divide with other expressions right") {
    val divide = Expr.Divide(
      Expr.Times(Expr.Num(1.0), Expr.Num(2.0)),
      Expr.Divide(Expr.Num(1.0), Expr.Num(2.0)),
    )
    assertEquals(divide.show, "(1.0 × 2.0) ÷ (1.0 ÷ 2.0)")
  }

  test("Show single Negate expression right") {
    val negate = Expr.Negate(Expr.Num(1.0))
    assertEquals(negate.show, "-1.0")
  }

  test("Show Negate with other expressions right") {
    val negate = Expr.Negate(
      Expr.Divide(
        Expr.Times(Expr.Num(1.0), Expr.Num(2.0)),
        Expr.Divide(Expr.Num(1.0), Expr.Num(2.0)),
      )
    )
    assertEquals(negate.show, "-((1.0 × 2.0) ÷ (1.0 ÷ 2.0))")
  }

  test("Show single Not expression right") {
    val not = Expr.Not(Expr.Bool(true))
    assertEquals(not.show, "!true")
  }

  test("Show Not with other expressions right") {
    val not = Expr.Not(
      Expr.And(
        Expr.Bool(true),
        Expr.Bool(false),
      )
    )
    assertEquals(not.show, "!(true and false)")
  }

  test("Show single Or expression right") {
    val or = Expr.Or(Expr.Bool(true), Expr.Bool(false))
    assertEquals(or.show, "true or false")
  }

  test("Show Or with other expressions right") {
    val or = Expr.Or(
      Expr.Not(
        Expr.Bool(true)
      ),
      Expr.Bool(false),
    )
    assertEquals(or.show, "(!true) or false")
  }
}
