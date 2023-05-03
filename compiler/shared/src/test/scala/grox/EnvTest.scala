package grox

import cats.effect.IO
import cats.syntax.all.*

import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF

import Arbitraries.given

class EnvTest extends CatsEffectSuite with ScalaCheckEffectSuite:

  test("define => get returns the same value"):
    PropF.forAllF { (name: String, value: LiteralType) =>
      Env
        .instance[IO](State())
        .flatMap(env =>
          for
            _ <- env.define(name, value)
            result <- env.get(name)
          yield assert(result == value)
        )
    }

  test("define => state"):
    PropF.forAllF { (xs: List[(String, LiteralType)]) =>
      val result = xs.foldLeft(Map[String, LiteralType]())((x, y) => x + y)
      Env
        .instance[IO](State())
        .flatMap(env =>
          for
            _ <- xs.traverse_((x, y) => env.define(x, y))
            state <- env.state
            map = state.values
          yield assert(map == result)
        )
    }

  test("define => define => get returns the later value"):
    PropF.forAllF { (name: String, v1: LiteralType, v2: LiteralType) =>
      Env
        .instance[IO](State())
        .flatMap(env =>
          for
            _ <- env.define(name, v1)
            _ <- env.define(name, v2)
            result <- env.get(name)
          yield assert(result == v2)
        )
    }

  test("define => assign => get returns the later value"):
    PropF.forAllF { (name: String, v1: LiteralType, v2: LiteralType) =>
      Env
        .instance[IO](State())
        .flatMap(env =>
          for
            _ <- env.define(name, v1)
            _ <- env.assign(name, v2)
            result <- env.get(name)
          yield assert(result == v2)
        )
    }

  test("assign value to a not declared variable returns error"):
    PropF.forAllF { (name: String, v: LiteralType) =>
      Env
        .instance[IO](State())
        .flatMap(_.assign(name, v))
        .as(false)
        .handleError(e => true)
        .map(assert(_))
    }

  test("get a value from empty Env returns error"):
    PropF.forAllF { (name: String) =>
      Env
        .instance[IO](State())
        .flatMap(_.get(name))
        .as(false)
        .handleError(e => true)
        .map(assert(_))
    }

  test("we can define a variable in inner block with the same name as outer block"):
    PropF.forAllF { (name: String, v1: LiteralType, v2: LiteralType) =>
      Env
        .instance[IO](State())
        .flatMap(env =>
          for
            _ <- env.define(name, v1)
            _ <- env.startBlock()
            _ <- env.define(name, v2)
            v21 <- env.get(name)
            _ <- env.endBlock()
            v11 <- env.get(name)
          yield assert(v11 == v1 && v21 == v2)
        )
    }

end EnvTest
