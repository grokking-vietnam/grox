package grox

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

import Interpreter.*
import Span.*

class InterpreterTest extends ScalaCheckSuite:

  property("addition") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Add(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(n1 + n2)
    }
  }
  property("addition 2 string") {
    forAll { (n1: String, n2: String) =>
      evaluate(Expr.Add(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(n1 + n2)
    }
  }

  property("subtraction") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Subtract(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(
        n1 - n2
      )
    }
  }
  property("multiplication") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Multiply(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(
        n1 * n2
      )
    }
  }
  property("division") {
    forAll { (n1: Double, n2: Double) =>
      n2 != 0 ==>
        (evaluate(Expr.Divide(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(
          n1 / n2
        ))
    }
  }

  property("greater") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Greater(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(
        n1 > n2
      )
    }
  }
  property("greater or equal") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.GreaterEqual(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(
        n1 >= n2
      )
    }
  }
  property("less") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Less(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(n1 < n2)
    }
  }
  property("less or equal") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.LessEqual(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(
        n1 <= n2
      )
    }
  }
  property("equal") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Equal(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(
        n1 == n2
      )
    }
  }
  property("not equal") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.NotEqual(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(
        n1 != n2
      )
    }
  }

  // from: https://www.learncbse.in/bodmas-rule/
  test("complex expression -4*(10+15/5*4-2*2)") {
    def evaluate(str: String) = Scanner
      .parse(str)
      .flatMap(Parser.parse(_))
      .flatMap(e => Interpreter.evaluate(e._1))
    val expr = evaluate("-4*(10+15/5*4-2*2)")
    val division = evaluate("-4*(10+3*4-2*2)")
    val multiplication = evaluate("-4*(10+12-4)")
    val addition = evaluate("-4*(22-4)")
    val subtraction = evaluate("-4*18")
    val answer = evaluate("-72")
    assertEquals(expr, division)
    assertEquals(division, multiplication)
    assertEquals(multiplication, addition)
    assertEquals(addition, subtraction)
    assertEquals(subtraction, answer)
  }

  test("division by zero error") {
    assertEquals(
      evaluate(Expr.Divide(empty, Expr.Literal(empty, 1), Expr.Literal(empty, 0))),
      Left(RuntimeError.DivisionByZero),
    )
  }

  test("logical or") {
    assertEquals(
      evaluate(Expr.Or(empty, Expr.Literal(empty, true), Expr.Literal(empty, false))),
      Right(true),
    )
    assertEquals(
      evaluate(Expr.Or(empty, Expr.Literal(empty, false), Expr.Literal(empty, false))),
      Right(false),
    )
  }

  test("logical and") {
    assertEquals(
      evaluate(Expr.And(empty, Expr.Literal(empty, true), Expr.Literal(empty, false))),
      Right(false),
    )
    assertEquals(
      evaluate(Expr.And(empty, Expr.Literal(empty, false), Expr.Literal(empty, false))),
      Right(false),
    )
  }

  test("must be numbers or strings runtime error") {
    assertEquals(
      evaluate(Expr.Add(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbersOrStrings),
    )
  }

  test("Two nulls should be equal") {
    assertEquals(
      evaluate(Expr.Equal(empty, Expr.Literal(empty, ()), Expr.Literal(empty, ()))),
      Right(true),
    )
  }

  test("Two operators which are different in type should not be equal") {
    assertEquals(
      evaluate(Expr.Equal(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Right(false),
    )
  }

  test("must be numbers runtime error") {
    assertEquals(
      evaluate(Expr.Subtract(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(Token.Minus(()))),
    )
    assertEquals(
      evaluate(Expr.Divide(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(Token.Slash(()))),
    )
    assertEquals(
      evaluate(Expr.Multiply(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(Token.Star(()))),
    )
    assertEquals(
      evaluate(Expr.Greater(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(Token.Greater(()))),
    )
    assertEquals(
      evaluate(Expr.GreaterEqual(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(Token.GreaterEqual(()))),
    )
    assertEquals(
      evaluate(Expr.Less(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(Token.Less(()))),
    )
    assertEquals(
      evaluate(Expr.LessEqual(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(Token.LessEqual(()))),
    )
  }
