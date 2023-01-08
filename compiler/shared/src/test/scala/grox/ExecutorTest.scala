package grox

import cats.Functor
import cats.effect.*
import scribe.cats.effect
import cats.syntax.all.*
import Token.*
import cats.effect.kernel.Resource
import munit.Clue.generate

class ExecutorTest extends munit.FunSuite:
  /*
  * I need help to create a new instance of `Executor[IO]`
  */
  var testee: Resource[IO, Executor[IO]] = Executor.module[IO]

  test("simple scan print") {
    val input = "print(\"Hello world\")"
    val scan: Resource[IO, IO[List[Token[Span]]]] = testee.map(exe => exe.scan(input))

//    result get from playground `print("Hello world")`
//    Print(Span(Location(0,0,0),Location(0,5,5))) LeftParen(Span(Location(0,5,5),Location(0,6,6))) Str(Hello world,Span(Location(0,6,6),Location(0,19,19))) RightParen(Span(Location(0,19,19),Location(0,20,20)))

    val expected: Resource[IO, IO[List[Token[Span]]]] = Resource.eval(
      IO.pure(
        IO{List(
          Print(Span(Location(0, 0, 0), Location(0, 5, 5))),
          LeftParen(Span(Location(0, 5, 5), Location(0, 6, 6))),
          Str("Hello world", Span(Location(0, 6, 6), Location(0, 19, 19))),
          RightParen(Span(Location(0, 19, 19), Location(0, 20, 20)))
        )
        }
      )
    )

    assertEquals(scan, expected)
  }

  test("simple parse print") {

  }
