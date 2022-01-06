package grox

import munit.CatsEffectSuite

import Expr._

class AstPrinterTest extends munit.FunSuite:
  test("Example 1 + 2") {
    assertEquals(
      AstPrinter.print(
        Add(Number(1), Number(2))
      ),
      "(+ 1 2)",
    )
  }

  test("Example -123*(45.67)") {
    assertEquals(
      AstPrinter.print(
        Multiply(Minus(Number(123)), Grouping(Number(45.67)))
      ),
      "(* (- 123) (group 45.67))",
    )
  }
