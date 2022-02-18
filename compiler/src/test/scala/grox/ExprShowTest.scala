package grox

import cats.implicits._

import ExprShow._

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

  test("Show single And expression right") {
    val and = Expr.And(Expr.Bool(true), Expr.Bool(false))
    assertEquals(and.show, "true && false")
  }

  test("Show single Plus expression right") {
    val and = Expr.And(Expr.Bool(true), Expr.Bool(false))
    assertEquals(and.show, "true && false")
  }
}
