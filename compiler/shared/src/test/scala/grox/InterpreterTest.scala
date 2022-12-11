package grox

import org.scalacheck.Prop.*
import munit.CatsEffectSuite
import munit.ScalaCheckEffectSuite

import Interpreter.*
import Span.*

class InterpreterTest extends CatsEffectSuite with ScalaCheckEffectSuite:

  given Env.instance[IO](State())
  val interpreter = Interpreter.instance[IO]
  val evaluate = Interpreter.evaluateWithState(State())

  property("addition") {
    forAll { (n1: Double, n2: Double) =>
      Env
        .instance[IO](State())
        .flatMap(env =>
          interpreter = Interpreter.instance(env)
          interpreter.evaluate(Expr.Add(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2))) == Right(
            n1 + n2
          )
        )
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

  test("variable expression") {
    val env = State(Map("x" -> 0.0d), None)
    val expr = Expr.Variable(empty, "x")
    assertEquals(interpreter.evaluate(env, expr), Right(0.0))
  }

  // from: https://www.learncbse.in/bodmas-rule/
  test("complex expression -4*(10+15/5*4-2*2)") {
    val env = State(Map("x" -> 10.0), None)

    def eval(str: String) = Scanner
      .parse(str)
      .flatMap(Parser.parse(_))
      .flatMap(x => interpreter.evaluate(env, x._1))

    val expr = eval("-4*(10+15/5*4-2*2)")
    val division = eval("-4*(10+3*4-2*2)")
    val multiplication = eval("-4*(10+12-4)")
    val addition = eval("-4*(22-4)")
    val subtraction = eval("-4*18")
    val answer = eval("-72")
    val exprWithX = eval("-4*(x+15/5*4-2*2)")
    val divisionWithX = eval("-4*(x+3*4-2*2)")

    assertEquals(expr, division)
    assertEquals(division, multiplication)
    assertEquals(multiplication, addition)
    assertEquals(addition, subtraction)
    assertEquals(subtraction, answer)
    assertEquals(exprWithX, answer)
    assertEquals(divisionWithX, answer)
  }

  test("division by zero error") {
    assertEquals(
      evaluate(Expr.Divide(empty, Expr.Literal(empty, 1), Expr.Literal(empty, 0))),
      Left(RuntimeError.DivisionByZero(empty)),
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
      Left(RuntimeError.MustBeNumbersOrStrings(empty)),
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
      Left(RuntimeError.MustBeNumbers(empty)),
    )
    assertEquals(
      evaluate(Expr.Divide(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(empty)),
    )
    assertEquals(
      evaluate(Expr.Multiply(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(empty)),
    )
    assertEquals(
      evaluate(Expr.Greater(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(empty)),
    )
    assertEquals(
      evaluate(Expr.GreaterEqual(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(empty)),
    )
    assertEquals(
      evaluate(Expr.Less(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(empty)),
    )
    assertEquals(
      evaluate(Expr.LessEqual(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string"))),
      Left(RuntimeError.MustBeNumbers(empty)),
    )
  }
