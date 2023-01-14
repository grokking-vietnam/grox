package grox

import cats.effect.IO
import cats.syntax.all.*

import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Prop.*
import org.scalacheck.effect.PropF.forAllF

import Interpreter.*
import Span.*
import Env.*

class InterpreterTest extends CatsEffectSuite with ScalaCheckEffectSuite:

  def evaluate(expr: Expr, state: State = State()): IO[LiteralType] =
    for
      given Env[IO] <- Env.instance[IO](state)
      interpreter = Interpreter.instance[IO]
      result <- interpreter.evaluate(expr)
    yield result

  test("addition") {
    forAllF { (n1: Double, n2: Double) =>
      evaluate(Expr.Add(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
        .map(x => assert(x == n1 + n2))
    }
  }

  test("addition 2 string") {
    forAllF { (n1: String, n2: String) =>
      evaluate(Expr.Add(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
        .map(x => assert(x == n1 + n2))
    }
  }

  test("subtraction") {
    forAllF { (n1: Double, n2: Double) =>
      evaluate(Expr.Subtract(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
        .map(x => assert(x == n1 - n2))
    }
  }

  test("multiplication") {
    forAllF { (n1: Double, n2: Double) =>
      evaluate(Expr.Multiply(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
        .map(x => assert(x == n1 * n2))
    }
  }
  test("division") {
    forAllF { (n1: Double, n2: Double) =>
      if n2 != 0 then
        evaluate(Expr.Divide(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
          .map(x => assert(x == n1 / n2))
      else IO(assert(true))
    }
  }

  test("greater") {
    forAllF { (n1: Double, n2: Double) =>
      evaluate(Expr.Greater(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
        .map(x => assert(x == n1 > n2))
    }
  }

  test("greater or equal") {
    forAllF { (n1: Double, n2: Double) =>
      evaluate(Expr.GreaterEqual(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
        .map(x => assert(x == n1 >= n2))
    }
  }
  test("less") {
    forAllF { (n1: Double, n2: Double) =>
      evaluate(Expr.Less(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
        .map(x => assert(x == n1 < n2))
    }
  }

  test("less or equal") {
    forAllF { (n1: Double, n2: Double) =>
      evaluate(Expr.LessEqual(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
        .map(x => assert(x == n1 <= n2))
    }
  }
  test("equal") {
    forAllF { (n1: Double, n2: Double) =>
      evaluate(Expr.Equal(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
        .map(x => assert(x == (n1 == n2)))
    }
  }
  test("not equal") {
    forAllF { (n1: Double, n2: Double) =>
      evaluate(Expr.NotEqual(empty, Expr.Literal(empty, n1), Expr.Literal(empty, n2)))
        .map(x => assert(x == (n1 != n2)))
    }
  }

  test("variable expression") {
    val state = State(Map("x" -> 0.0d), None)
    val expr = Expr.Variable(empty, "x")
    evaluate(expr, state).map(x => assert(x == 0.0d))
  }

  // from: https://www.learncbse.in/bodmas-rule/
  test("complex expression -4*(10+15/5*4-2*2)") {
    val env = State(Map("x" -> 10.0), None)

    def eval(str: String) = Scanner
      .parse(str)
      .flatMap(Parser.parse(_))
      .liftTo[IO]
      .flatMap(x => evaluate(x._1, env))

    val expr = eval("-4*(10+15/5*4-2*2)")
    val division = eval("-4*(10+3*4-2*2)")
    val multiplication = eval("-4*(10+12-4)")
    val addition = eval("-4*(22-4)")
    val subtraction = eval("-4*18")
    val answer = eval("-72")
    val exprWithX = eval("-4*(x+15/5*4-2*2)")
    val divisionWithX = eval("-4*(x+3*4-2*2)")

    (expr, division).mapN((x, y) => assert(x == y))
    (division, multiplication).mapN((x, y) => assert(x == y))
    (multiplication, addition).mapN((x, y) => assert(x == y))
    (addition, subtraction).mapN((x, y) => assert(x == y))
    (subtraction, answer).mapN((x, y) => assert(x == y))
    (exprWithX, answer).mapN((x, y) => assert(x == y))
    (divisionWithX, answer).mapN((x, y) => assert(x == y))
  }

  test("division by zero error") {
    evaluate(Expr.Divide(empty, Expr.Literal(empty, 1), Expr.Literal(empty, 0)))
      .attempt
      .map(x => assert(x == Left(RuntimeError.DivisionByZero(empty))))
  }

  test("logical or") {
    evaluate(Expr.Or(empty, Expr.Literal(empty, true), Expr.Literal(empty, false)))
      .map(x => assert(x == true))
    evaluate(Expr.Or(empty, Expr.Literal(empty, false), Expr.Literal(empty, false)))
      .map(x => assert(x == false))
  }

  test("logical and") {
    evaluate(Expr.And(empty, Expr.Literal(empty, true), Expr.Literal(empty, false)))
      .map(x => assert(x == false))
    evaluate(Expr.And(empty, Expr.Literal(empty, false), Expr.Literal(empty, false)))
      .map(x => assert(x == false))
  }

  test("must be numbers or strings runtime error") {
    evaluate(Expr.Add(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string")))
      .attempt
      .map(x => assert(x == Left(RuntimeError.MustBeNumbersOrStrings(empty))))
  }

  test("Two nulls should be equal") {
    evaluate(Expr.Equal(empty, Expr.Literal(empty, ()), Expr.Literal(empty, ())))
      .map(x => assert(x == true))
  }

  test("Two operators which are different in type should not be equal") {
    evaluate(Expr.Equal(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string")))
      .map(x => assert(x == false))
  }

  test("must be numbers runtime error") {
    evaluate(Expr.Subtract(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string")))
      .attempt
      .map(x => assert(x == Left(RuntimeError.MustBeNumbers(empty))))

    evaluate(Expr.Divide(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string")))
      .attempt
      .map(x => assert(x == Left(RuntimeError.MustBeNumbers(empty))))

    evaluate(Expr.Multiply(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string")))
      .attempt
      .map(x => assert(x == Left(RuntimeError.MustBeNumbers(empty))))

    evaluate(Expr.Greater(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string")))
      .attempt
      .map(x => assert(x == Left(RuntimeError.MustBeNumbers(empty))))

    evaluate(Expr.GreaterEqual(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string")))
      .attempt
      .map(x => assert(x == Left(RuntimeError.MustBeNumbers(empty))))

    evaluate(Expr.Less(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string")))
      .attempt
      .map(x => assert(x == Left(RuntimeError.MustBeNumbers(empty))))

    evaluate(Expr.LessEqual(empty, Expr.Literal(empty, 1), Expr.Literal(empty, "string")))
      .attempt
      .map(x => assert(x == Left(RuntimeError.MustBeNumbers(empty))))
  }
