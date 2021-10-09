package grox

import munit.CatsEffectSuite

class Test extends CatsEffectSuite {
  test("hello") {
    assertEquals(
      1 + 1,
      2,
    )
  }
}
