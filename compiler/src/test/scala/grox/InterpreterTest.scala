package grox

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

import Interpreter.*

class InterpreterTest extends ScalaCheckSuite:

  property("addition") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Add(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1 + n2)
    }
  }

  property("subtraction") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Subtract(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1 - n2)
    }
  }
  property("multiplication") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Multiply(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1 * n2)
    }
  }
  property("division") {
    forAll { (n1: Double, n2: Double) =>
      n2 != 0 ==>
        (evaluate(Expr.Divide(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1 / n2))
    }
  }

  test("addition runtime error") {
    assertEquals(
      evaluate(Expr.Add(Expr.Literal(1), Expr.Literal("string"))),
      Left(RuntimeError.MustBeNumbersOrStrings),
    )
  }

  test("division by zero error") {
    assertEquals(
      evaluate(Expr.Divide(Expr.Literal(1), Expr.Literal(0))),
      Left(RuntimeError.DivisionByZero),
    )
  }
