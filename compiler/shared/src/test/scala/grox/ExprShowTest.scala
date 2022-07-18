package grox

import cats.implicits.*

import Expr.*
import Span.*

class ExprShowTest extends munit.FunSuite:
  val span = Span(Location(0, 0, 0), Location(0, 5, 5))

  test("Show single Add expression right") {
     val add = Expr.Add(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0))
     assertEquals(add.show, "1.0 + 2.0")
   }

   test("Show Add with other expressions right") {
     val add = Expr.Add(
       empty,
       Expr.Add(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0)),
       Expr.Subtract(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0)),
     )
     assertEquals(add.show, "(1.0 + 2.0) + (1.0 - 2.0)")
   }

   test("Show single Subtract expression right") {
     val subtract = Expr.Subtract(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0))
     assertEquals(subtract.show, "1.0 - 2.0")
   }

   test("Show Subtract with other expressions right") {
     val subtract = Expr.Subtract(
       empty,
       Expr.Add(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0)),
       Expr.Subtract(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0)),
     )
     assertEquals(subtract.show, "(1.0 + 2.0) - (1.0 - 2.0)")
   }

   test("Show single Multiply expression right") {
     val multiply = Expr.Multiply(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0))
     assertEquals(multiply.show, "1.0 × 2.0")
   }

   test("Show Multiply with other expressions right") {
     val multiply = Expr.Multiply(
       empty,
       Expr.Add(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0)),
       Expr.Subtract(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0)),
     )
     assertEquals(multiply.show, "(1.0 + 2.0) × (1.0 - 2.0)")
   }

   test("Show single Divide expression right") {
     val divide = Expr.Divide(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0))
     assertEquals(divide.show, "1.0 ÷ 2.0")
   }

   test("Show Divide with other expressions right") {
     val divide = Expr.Divide(
       empty,
       Expr.Multiply(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0)),
       Expr.Divide(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0)),
     )
     assertEquals(divide.show, "(1.0 × 2.0) ÷ (1.0 ÷ 2.0)")
   }

   test("Show single Negate expression right") {
     val negate = Expr.Negate(
       empty,
       Expr.Literal(empty, 1.0)
     )
     assertEquals(negate.show, "-1.0")
   }

   test("Show Negate with other expressions right") {
     val negate = Expr.Negate(
       empty,
       Expr.Divide(
         empty,
         Expr.Multiply(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0)),
         Expr.Divide(empty, Expr.Literal(empty, 1.0), Expr.Literal(empty, 2.0)),
       )
     )
     assertEquals(negate.show, "-((1.0 × 2.0) ÷ (1.0 ÷ 2.0))")
   }

   test("Show single Not expression right") {
     val not = Expr.Not(empty, Expr.Literal(empty, true))
     assertEquals(not.show, "!true")
   }

   test("logical or") {
     val or = Expr.Or(empty, Expr.Literal(empty, true), Expr.Literal(empty, false))
     assertEquals(or.show, "true or false")
   }

   test("logical and") {
     val and = Expr.And(span, Expr.Literal(span, true), Expr.Literal(span, false))
     assertEquals(and.show, "true and false")
   }

   test("1 + nil") {
     val onePlusNil = Expr.Add(span, Expr.Literal(span, 1), Expr.Literal(span, ()))
     assertEquals(onePlusNil.show, "1.0 + nil")
   }
