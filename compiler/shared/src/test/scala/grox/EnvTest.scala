package grox

import cats.effect.IO

import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.effect.PropF

import Arbitraries.given

class EnvTest extends CatsEffectSuite with ScalaCheckEffectSuite:

  test("define => get returns the same value") {
    PropF.forAllF { (name: String, value: LiteralType) =>
      Env
        .instance[IO](Environment())
        .flatMap(env =>
          for
            _ <- env.define(name, value)
            result <- env.get(name)
          yield assert(result == value)
        )
    }
  }

  test("define => assign => get returns the later value") {
    PropF.forAllF { (name: String, v1: LiteralType, v2: LiteralType) =>
      Env
        .instance[IO](Environment())
        .flatMap(env =>
          for
            _ <- env.define(name, v1)
            _ <- env.define(name, v2)
            result <- env.get(name)
          yield assert(result == v2)
        )
    }
  }

end EnvTest
