package grox

import cats.implicits.*

import Expr.*

class ExprShowTest extends munit.FunSuite:
  test("Show single Add expression right") {
    val add = Expr.Add(Expr.Literal(1.0), Expr.Literal(2.0))
    assertEquals(add.show, "1.0 + 2.0")
  }

  test("Show Add with other expressions right") {
    val add = Expr.Add(
      Expr.Add(Expr.Literal(1.0), Expr.Literal(2.0)),
      Expr.Subtract(Expr.Literal(1.0), Expr.Literal(2.0)),
    )
    assertEquals(add.show, "(1.0 + 2.0) + (1.0 - 2.0)")
  }

  test("Show single Subtract expression right") {
    val subtract = Expr.Subtract(Expr.Literal(1.0), Expr.Literal(2.0))
    assertEquals(subtract.show, "1.0 - 2.0")
  }

  test("Show Subtract with other expressions right") {
    val subtract = Expr.Subtract(
      Expr.Add(Expr.Literal(1.0), Expr.Literal(2.0)),
      Expr.Subtract(Expr.Literal(1.0), Expr.Literal(2.0)),
    )
    assertEquals(subtract.show, "(1.0 + 2.0) - (1.0 - 2.0)")
  }

  test("Show single Multiply expression right") {
    val multiply = Expr.Multiply(Expr.Literal(1.0), Expr.Literal(2.0))
    assertEquals(multiply.show, "1.0 × 2.0")
  }

  test("Show Multiply with other expressions right") {
    val multiply = Expr.Multiply(
      Expr.Add(Expr.Literal(1.0), Expr.Literal(2.0)),
      Expr.Subtract(Expr.Literal(1.0), Expr.Literal(2.0)),
    )
    assertEquals(multiply.show, "(1.0 + 2.0) × (1.0 - 2.0)")
  }

  test("Show single Divide expression right") {
    val divide = Expr.Divide(Expr.Literal(1.0), Expr.Literal(2.0))
    assertEquals(divide.show, "1.0 ÷ 2.0")
  }

  test("Show Divide with other expressions right") {
    val divide = Expr.Divide(
      Expr.Multiply(Expr.Literal(1.0), Expr.Literal(2.0)),
      Expr.Divide(Expr.Literal(1.0), Expr.Literal(2.0)),
    )
    assertEquals(divide.show, "(1.0 × 2.0) ÷ (1.0 ÷ 2.0)")
  }

  test("Show single Negate expression right") {
    val negate = Expr.Negate(Expr.Literal(1.0))
    assertEquals(negate.show, "-1.0")
  }

  test("Show Negate with other expressions right") {
    val negate = Expr.Negate(
      Expr.Divide(
        Expr.Multiply(Expr.Literal(1.0), Expr.Literal(2.0)),
        Expr.Divide(Expr.Literal(1.0), Expr.Literal(2.0)),
      )
    )
    assertEquals(negate.show, "-((1.0 × 2.0) ÷ (1.0 ÷ 2.0))")
  }

  test("Show single Not expression right") {
    val not = Expr.Not(Expr.Literal(true))
    assertEquals(not.show, "!true")
  }

  test("logical or") {
    val or = Expr.Or(Expr.Literal(true), Expr.Literal(false))
    assertEquals(or.show, "true or false")
  }

  test("logical and") {
    val and = Expr.And(Expr.Literal(true), Expr.Literal(false))
    assertEquals(and.show, "true and false")
  }

  test("1 + nil") {
    val onePlusNil = Expr.Add(Expr.Literal(1), Expr.Literal(()))
    assertEquals(onePlusNil.show, "1.0 + nil")
  }
