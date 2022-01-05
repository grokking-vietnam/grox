package grox

import grox.Expr._
import grox.{AstPrinter, Token}
import munit.CatsEffectSuite

class AstPrinterTest extends munit.FunSuite:
  test("Example 1 + 2") {
    assertEquals(
      AstPrinter.print(
        Binary(
          Literal(MyToken.Number("1")),
          MyToken.Plus,
          Literal(MyToken.Number("2")),
        )
      ),
      "(+ 1 2)",
    )
  }

  test("Example -123*(45.67)") {
    assertEquals(
      AstPrinter.print(
        Binary(
          Unary(
            MyToken.Minus,
            Literal(MyToken.Number("123")),
          ),
          MyToken.Star,
          Grouping(Literal(MyToken.Number("45.67"))),
        )
      ),
      "(* (- 123) (group 45.67))",
    )
  }
