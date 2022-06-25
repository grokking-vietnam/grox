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
  property("addition 2 string") {
    forAll { (n1: String, n2: String) =>
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

  property("greater") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Greater(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1 > n2)
    }
  }
  property("greater or equal") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.GreaterEqual(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1 >= n2)
    }
  }
  property("less") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Less(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1 < n2)
    }
  }
  property("less or equal") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.LessEqual(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1 <= n2)
    }
  }
  property("equal") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.Equal(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1 == n2)
    }
  }
  property("not equal") {
    forAll { (n1: Double, n2: Double) =>
      evaluate(Expr.NotEqual(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1 != n2)
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
      evaluate(Expr.Divide(Expr.Literal(1), Expr.Literal(0))),
      Left(RuntimeError.DivisionByZero),
    )
  }

  test("must be numbers or strings runtime error") {
    assertEquals(
      evaluate(Expr.Add(Expr.Literal(1), Expr.Literal("string"))),
      Left(RuntimeError.MustBeNumbersOrStrings),
    )
  }

  test("Two nulls should be equal") {
    assertEquals(evaluate(Expr.Equal(Expr.Literal(null), Expr.Literal(null))), Right(true))
  }

  test("Two operators which are different in type should not be equal") {
    assertEquals(evaluate(Expr.Equal(Expr.Literal(1), Expr.Literal("string"))), Right(false))
  }

  test("must be numbers runtime error") {
    assertEquals(
      evaluate(Expr.Subtract(Expr.Literal(1), Expr.Literal("string"))),
      Left(RuntimeError.MustBeNumbers(Token.Minus(()))),
    )
    assertEquals(
      evaluate(Expr.Divide(Expr.Literal(1), Expr.Literal("string"))),
      Left(RuntimeError.MustBeNumbers(Token.Slash(()))),
    )
    assertEquals(
      evaluate(Expr.Multiply(Expr.Literal(1), Expr.Literal("string"))),
      Left(RuntimeError.MustBeNumbers(Token.Star(()))),
    )
    assertEquals(
      evaluate(Expr.Greater(Expr.Literal(1), Expr.Literal("string"))),
      Left(RuntimeError.MustBeNumbers(Token.Greater(()))),
    )
    assertEquals(
      evaluate(Expr.GreaterEqual(Expr.Literal(1), Expr.Literal("string"))),
      Left(RuntimeError.MustBeNumbers(Token.GreaterEqual(()))),
    )
    assertEquals(
      evaluate(Expr.Less(Expr.Literal(1), Expr.Literal("string"))),
      Left(RuntimeError.MustBeNumbers(Token.Less(()))),
    )
    assertEquals(
      evaluate(Expr.LessEqual(Expr.Literal(1), Expr.Literal("string"))),
      Left(RuntimeError.MustBeNumbers(Token.LessEqual(()))),
    )
  }
