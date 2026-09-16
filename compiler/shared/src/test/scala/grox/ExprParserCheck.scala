package grox

import cats.effect.IO
import org.scalacheck.effect.PropF.forAllF
import cats.syntax.all.*

import org.scalacheck.Prop

import TokenParser.*
import ExprGen.*

class ExprParserCheck extends munit.CatsEffectSuite with munit.ScalaCheckSuite:

  val parse = run(ExprParser.expr)

  property("parse numerics succesfully"):
    Prop.forAll(numericGen) { expr =>
      parse(expr.flatten) match
        case Left(_)  => false
        case Right(_) => true
    }

  property("parse logicals succesfully"):
    Prop.forAll(logicalGen) { expr =>
      parse(expr.flatten) match
        case Left(_)  => false
        case Right(_) => true
    }

  def evaluate(expr: Expr): IO[LiteralType] =
    for
      given Env[IO] <- Env.instance[IO](State())
      interpreter = Interpreter.instance[IO]
      result <- interpreter.evaluate(expr)
    yield result

  test("produce an equal numeric expression"):
    forAllF(numericGen) { expr =>
      parse(expr.flatten) match
        case Left(_)           => IO(assert(false))
        case Right(parsedExpr) => (evaluate(expr).attempt, evaluate(parsedExpr).attempt).mapN:
            case (Left(e1), Left(e2))                   => assert(e1 == e2)
            case (Right(v1: Double), Right(v2: Double)) => assert(math.abs(v1 - v2) < 0.01)
            case _                                      => assert(false)
    }

  test("produce an equal logical expression"):
    forAllF(logicalGen) { expr =>
      parse(expr.flatten) match
        case Left(_)           => IO(assert(false))
        case Right(parsedExpr) =>
          (evaluate(expr), evaluate(parsedExpr)).mapN((x, y) => assert(x == y))
    }
