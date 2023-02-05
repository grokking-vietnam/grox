package grox

import cats.Functor
import cats.effect.*
import scribe.cats.effect
import cats.syntax.all.*
import Token.*
import Span.*
import cats.effect.kernel.Resource
import grox.Scanner.Error
import grox.Parser.Error
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import munit.Clue.generate
import cats.parse.LocationMap

class ExecutorTesting extends CatsEffectSuite with ScalaCheckEffectSuite :
  /*
  * I need help to create a new instance of `Executor[IO]`
  */
  var testee: Resource[IO, Executor[IO]] = Executor.module[IO]

  test("simple scan print should be success") {
    val input = """print("Hello world")"""
    val expectedResult: List[Token[Span]] = List(
      Print(Span(Location(0, 0, 0), Location(0, 5, 5))),
      LeftParen(Span(Location(0, 5, 5), Location(0, 6, 6))),
      Str("Hello world", Span(Location(0, 6, 6), Location(0, 19, 19))),
      RightParen(Span(Location(0, 19, 19), Location(0, 20, 20)))
    )
    testee.use(exe => exe.scan(input)).map { result =>
      assertEquals(result, expectedResult)
    }
  }

  test("Scan print helloworld without ')' should succeed without error") {
    val input = """print("Hello world""""
    val expectedResult: List[Token[Span]] = List(
      Print(Span(Location(0, 0, 0), Location(0, 5, 5))),
      LeftParen(Span(Location(0, 5, 5), Location(0, 6, 6))),
      Str("Hello world", Span(Location(0, 6, 6), Location(0, 19, 19)))
    )
    testee.use(exe => exe.scan(input)).map { result =>
      assertEquals(result, expectedResult)
    }
  }

  test("Scan with a single string should accepted") {
    val input = """A"""
    val expectedResult: List[Token[Span]] = List(
      Identifier("A", Span(Location(0, 0, 0), Location(0, 1, 1)))
    )
    testee.use(exe => exe.scan(input)).map { result =>
      assertEquals(result, expectedResult)
    }
  }

  test("Scan with a 'space' should not accepted") {
    val input = """ """
    val expectedResult: Scanner.Error.ParseFailure = Scanner.Error.ParseFailure(1, LocationMap(" "))
    testee.use(exe => exe.scan(input)).handleError {
      case err: Scanner.Error.ParseFailure =>
        assertEquals(err.position, expectedResult.position)
        assertEquals(err.locations.input, expectedResult.locations.input)
    }
  }

//  test("Simple evaluate print should be success") {
//    val input =
//      """
//        |for (var i = 0; i < 10; i = i + 1) print i;
//        |""".stripMargin
//    val expectedResult: Scanner.Error.ParseFailure = Scanner.Error.ParseFailure(1, LocationMap(" "))
//    testee.use(exe => exe.evaluate(input)).handleError {
//      case err: Parser.Error.ExpectExpression =>
//      assertEquals(err, expectedResult)
//    }
//  }

  test("Parser 'print' stmt without ')' should error") {
    val input = """print("Hello world""""
    val expectedResult: Parser.Error.ExpectClosing = Parser.Error.ExpectClosing(List(Print(Span(Location(0,0,0),Location(0,5,5))), LeftParen(Span(Location(0,5,5),Location(0,6,6))), Str("Hello world",Span(Location(0,6,6),Location(0,19,19)))))
    testee.use(exe => exe.parse(input)).handleError {
      case err: Parser.Error.ExpectExpression =>
        assert("Expect ')' after expression".equals(err.getMessage))
    }
  }

  test("Parser 'print' stmt with wrong closing '}' should error") {
    val input = """print("Hello world""""
    val expectedResult: Parser.Error.ExpectClosing = Parser.Error.ExpectClosing(List(Print(Span(Location(0, 0, 0), Location(0, 5, 5))), LeftParen(Span(Location(0, 5, 5), Location(0, 6, 6))), Str("Hello world", Span(Location(0, 6, 6), Location(0, 19, 19))), RightBrace(Span(Location(0,19,19), Location(0,20,20)))))
    testee.use(exe => exe.parse(input)).handleError {
      case err: Parser.Error.ExpectExpression =>
        assert("Expect ')' after expression".equals(err.getMessage))
    }
  }