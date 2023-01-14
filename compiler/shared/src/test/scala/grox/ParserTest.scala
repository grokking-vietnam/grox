package grox

import cats.effect.IO
import cats.syntax.all.*

import munit.ScalaCheckSuite
import org.scalacheck.effect.PropF.forAllF
import org.scalacheck.{Arbitrary, Gen, Prop}

import Parser.*
import ExprGen.*
import Token.*
import Span.*

class ParserTest extends munit.CatsEffectSuite with ScalaCheckSuite:

  val num1 = Number("1", empty)
  val num2 = Number("2", empty)
  val num3 = Number("3", empty)
  val num4 = Number("4", empty)
  val num5 = Number("5", empty)
  val num6 = Number("6", empty)
  val num42 = Number("42", empty)

  val expr1 = Expr.Literal(empty, 1)
  val expr2 = Expr.Literal(empty, 2)
  val expr3 = Expr.Literal(empty, 3)
  val expr4 = Expr.Literal(empty, 4)
  val expr5 = Expr.Literal(empty, 5)
  val expr6 = Expr.Literal(empty, 6)
  val avar: Identifier[Span] = Identifier("a", empty)

  val expr42 = Expr.Literal(empty, 42)

  test("empty") {
    assertEquals(parse(Nil), Left(Error.ExpectExpression(Nil)))
  }

  test("primary number") {
    val ts = List(Number("42", empty))
    assertEquals(parse(ts), Right(expr42, Nil))
  }

  test("primary string") {
    val ts = List(Str("you rox!", empty))
    val want = Expr.Literal(empty, "you rox!")
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("primary true") {
    val ts = List(True(empty))
    val want = Expr.Literal(empty, true)
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("primary true") {
    val ts = List(False(empty))
    val want = Expr.Literal(empty, false)
    assertEquals(parse(ts), Right(want, List()))
  }

  test("primary nil") {
    val ts = List(Null(empty))
    val want = Expr.Literal(empty, ())
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("primary variable") {
    val avar: Identifier[Span] = Identifier("a", empty)
    val ts = List(avar)
    val want = Expr.Variable(empty, "a")
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("unary negate") {
    val ts = List(Minus(empty), Number("42", empty))
    val want = Expr.Negate(empty, expr42)
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("unary not") {
    val ts = List(Bang(empty), False(empty))
    val want = Expr.Not(empty, Expr.Literal(empty, false))
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("unary multiple minus") {
    val ts = List(Minus(empty), Minus(empty), Minus(empty), Number("1", empty))
    val want = Expr.Negate(empty, Expr.Negate(empty, Expr.Negate(empty, Expr.Literal(empty, 1))))
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("factor 2 numbers") {
    val ts = List(num2, Star(empty), num5)
    val want = Expr.Multiply(empty, expr2, expr5)
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("factor 4 numbers") {
    // 2 * 5 * 1 / 42
    val ts = List(num2, Star(empty), num5, Star(empty), num1, Slash(empty), num42)
    val want = Expr.Divide(
      empty,
      Expr.Multiply(empty, Expr.Multiply(empty, expr2, expr5), expr1),
      expr42,
    )
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("factor with unary") {
    // -1 * 2 * -3
    val ts = List(Minus(empty), num1, Star(empty), num2, Star(empty), Minus(empty), num3)
    val want = Expr.Multiply(
      empty,
      Expr.Multiply(empty, Expr.Negate(empty, expr1), expr2),
      Expr.Negate(empty, expr3),
    )
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("term 2 numbers") {
    val ts = List(num2, Minus(empty), Minus(empty), num3)
    val want = Expr.Subtract(empty, expr2, Expr.Negate(empty, expr3))
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("term 4 numbers") {
    // 1 * -2 * 3 - 4
    val ts = List(
      num1,
      Plus(empty),
      Minus(empty),
      num2,
      Star(empty),
      num3,
      Minus(empty),
      num4,
    )
    val want = Expr.Subtract(
      empty,
      Expr.Add(
        empty,
        expr1,
        Expr.Multiply(empty, Expr.Negate(empty, expr2), expr3),
      ),
      expr4,
    )
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("comparison") {
    // 1 * 2 > -3 * 4
    val ts = List(
      num1,
      Star(empty),
      num2,
      Greater(empty),
      Minus(empty),
      num3,
      Plus(empty),
      num4,
    )
    val want = Expr.Greater(
      empty,
      Expr.Multiply(empty, expr1, expr2),
      Expr.Add(empty, Expr.Negate(empty, expr3), expr4),
    )
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("equality") {
    // false != 1 * 2 > -3 * 4
    val ts = List(
      False(empty),
      BangEqual(empty),
      num1,
      Star(empty),
      num2,
      Greater(empty),
      Minus(empty),
      num3,
      Plus(empty),
      num4,
    )
    val want = Expr.NotEqual(
      empty,
      Expr.Literal(empty, false),
      Expr.Greater(
        empty,
        Expr.Multiply(empty, expr1, expr2),
        Expr.Add(empty, Expr.Negate(empty, expr3), expr4),
      ),
    )
    assertEquals(parse(ts), Right(want, Nil))
  }

  test("combination") {
    // false == !(1 * 2 > -3 + 4 / 5)
    val ts = List(
      False(empty),
      EqualEqual(empty),
      Bang(empty),
      LeftParen(empty),
      num1,
      Star(empty),
      num2,
      Greater(empty),
      Minus(empty),
      num3,
      Plus(empty),
      num4,
      Slash(empty),
      num5,
      RightParen(empty),
      num1,
      EqualEqual(empty),
      num2,
    )
    val want = Expr.Equal(
      empty,
      Expr.Literal(empty, false),
      Expr.Not(
        empty,
        Expr.Grouping(
          Expr.Greater(
            empty,
            Expr.Multiply(empty, expr1, expr2),
            Expr.Add(
              empty,
              Expr.Negate(empty, expr3),
              Expr.Divide(empty, expr4, expr5),
            ),
          )
        ),
      ),
    )
    val rmn = List(num1, EqualEqual(empty), num2)

    assertEquals(parse(ts), Right(want, rmn))
  }

  test("AND OR logic") {
    // True OR (3 >= 4) AND (5 < 6)
    val ts = List(
      True(empty),
      Or(empty),
      LeftParen(empty),
      num3,
      GreaterEqual(empty),
      num4,
      RightParen(empty),
      And(empty),
      LeftParen(empty),
      num5,
      Less(empty),
      num6,
      RightParen(empty),
    )
    val want = Expr.Or(
      empty,
      left = Expr.Literal(empty, true),
      right = Expr.And(
        empty,
        left = Expr.Grouping(
          Expr.GreaterEqual(
            empty,
            Expr.Literal(empty, 3),
            Expr.Literal(empty, 4),
          )
        ),
        right = Expr.Grouping(
          Expr.Less(
            empty,
            Expr.Literal(empty, 5),
            Expr.Literal(empty, 6),
          )
        ),
      ),
    )

    assertEquals(parse(ts), Right(want, Nil))

  }

  test("error: expect expression") {
    // 1 + 2 / (3 - )
    val ts = List(
      num1,
      Plus(empty),
      num2,
      Slash(empty),
      LeftParen(empty),
      num3,
      Minus(empty),
      RightParen(empty),
    )
    assertEquals(parse(ts), Left(Error.ExpectExpression(List(RightParen(empty)))))
  }

  test("error: expect closing paren") {
    // 1 + 2 / (3 - 4  true false
    val ts = List(
      num1,
      Plus(empty),
      num2,
      Slash(empty),
      LeftParen(empty),
      num3,
      Minus(empty),
      num4,
      True(empty),
      False(empty),
    )
    assertEquals(parse(ts), Left(Error.ExpectClosing(List(True(empty), False(empty)))))
  }

  test("synchronize: until statement") {
    // a = a + 1) { print a; }
    val ts = List(
      avar,
      Equal(empty),
      avar,
      Plus(empty),
      avar,
      RightParen(empty),
      LeftBrace(empty),
      Print(empty),
      avar,
      Semicolon(empty),
      RightBrace(empty),
    )
    val remaining = ts.dropWhile(_ != Print(empty))
    assertEquals(synchronize(ts), remaining)
  }

  test("synchronize: until new expression") {
    // + 1; 2 * 3;
    val ts = List(
      Plus(empty),
      num1,
      Semicolon(empty),
      num2,
      Star(empty),
      num3,
      Semicolon(empty),
    )
    val remaining = ts.dropWhile(_ != num2)
    assertEquals(synchronize(ts), remaining)
  }

  test("Val declaration") {
    val ts = List(
      Var(empty),
      avar,
      Equal(empty),
      num42,
      Semicolon(empty),
    )

    val expectedStmt = Stmt.Var(
      avar,
      Some(expr42),
    )

    val inspector = Inspector().copy(tokens = ts)

    val expectedInspector = Inspector().copy(stmts = List(expectedStmt))

    assertEquals(_parseStmt(inspector), expectedInspector)
  }

  test("While: statement ") {
    val ts = List(
      While(empty),
      LeftParen(empty),
      True(empty),
      RightParen(empty),
      LeftBrace(empty),

      // val a = 42
      Var(empty),
      avar,
      Equal(empty),
      num42,
      Semicolon(empty),
      // a = a + a
      avar,
      Equal(empty),
      avar,
      Plus(empty),
      avar,
      Semicolon(empty),
      RightBrace(empty),
    )

    val expectedStmt = Stmt.While(
      Expr.Literal(empty, true),
      Stmt.Block(
        List(
          Stmt.Var(
            avar,
            Some(expr42),
          ),
          Stmt.Assign(
            "a",
            Expr.Add(
              empty,
              Expr.Variable(empty, "a"),
              Expr.Variable(empty, "a"),
            ),
          ),
        )
      ),
    )

    val inspector: Inspector = Inspector(
      List.empty[Error],
      List.empty[Stmt],
      tokens = ts,
    )
    val expectedInspector = inspector.copy(
      stmts = List(expectedStmt),
      tokens = Nil,
    )

    assertEquals(_parseStmt(inspector), expectedInspector)

  }

  test("1 + nil") {
    val ts = List(Number("1", empty), Plus(empty), Null(empty))
    val want = Expr.Add(empty, Expr.Literal(empty, 1), Expr.Literal(empty, ()))
    assertEquals(parse(ts), Right(want, Nil))
  }

end ParserTest

class ParserCheck extends ScalaCheckSuite:
  property("parse numerics succesfully") {
    Prop.forAll(numericGen) { expr =>
      parse(expr.flatten) match
        case Left(_)  => false
        case Right(_) => true
    }
  }

  property("parse logicals succesfully") {
    Prop.forAll(logicalGen) { expr =>
      parse(expr.flatten) match
        case Left(_)  => false
        case Right(_) => true
    }
  }

  def evaluate(expr: Expr, state: State = State()): IO[LiteralType] =
    for
      given Env[IO] <- Env.instance[IO](State())
      interpreter = Interpreter.instance[IO]
      result <- interpreter.evaluate(expr)
    yield result

  test("produce an equal numeric expression") {
    forAllF(numericGen) { expr =>
      parse(expr.flatten) match
        case Left(_) => IO(assert(false))
        case Right(parsedExpr, _) =>
          (evaluate(expr).attempt, evaluate(parsedExpr).attempt).mapN {
            case (Left(e1), Left(e2))                   => assert(e1 == e2)
            case (Right(v1: Double), Right(v2: Double)) => assert(math.abs(v1 - v2) < 0.01)
            case _                                      => assert(false)
          }
    }
  }

  test("produce an equal logical expression") {
    forAllF(logicalGen) { expr =>
      parse(expr.flatten) match
        case Left(_) => IO(assert(false))
        case Right(parsedExpr, _) =>
          (evaluate(expr), evaluate(parsedExpr)).mapN((x, y) => assert(x == y))
    }
  }

end ParserCheck
