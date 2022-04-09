package grox

import org.scalacheck.Prop.*
import munit.ScalaCheckSuite
import Interpreter.*

class InterpreterTest extends ScalaCheckSuite:

    property("addition") {
        forAll { (n1: Double, n2: Double) =>
            evaluate(Expr.Add(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1+n2)
        }
    }

    property("subtraction") {
        forAll { (n1: Double, n2: Double) =>
            evaluate(Expr.Subtract(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1-n2)
        }
    }
    property("multiplication") {
        forAll { (n1: Double, n2: Double) =>
            evaluate(Expr.Multiply(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1*n2)
        }
    }
    property("division") {
        forAll { (n1: Double, n2: Double) =>
            evaluate(Expr.Divide(Expr.Literal(n1), Expr.Literal(n2))) == Right(n1/n2)
        }
    }

    test("addition runtime error") {
        assertEquals(evaluate(Expr.Add(Expr.Literal(1), Expr.Literal("string"))), Left(RuntimeError.MustBeNumbersOrStrings))
    }