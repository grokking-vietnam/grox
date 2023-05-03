package grox

import tyrian.Cmd
import cats.effect.IO
import munit.Clue.generate

class PlaygroundTests extends munit.FunSuite:
  test("dummy test") {
    assert(1 == 1)
  }

  test("simple print 'hello world'") {
    val input = "Hello world"
    val scan: Cmd[IO, Msg] = Playground.scan(input)
    val parse: Cmd[IO, Msg] = Playground.parse(input)
    println(scan)
    println(parse)
    // todo add assert for it
  }
