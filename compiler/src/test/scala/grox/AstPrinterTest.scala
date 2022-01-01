package grox

import munit.CatsEffectSuite
import grox.AstPrinter
import grox.Expr._
import grox.Token

class AstPrinterTest extends munit.FunSuite:
  test("Example 1 + 2") {
    assertEquals(
      AstPrinter.print(
        Binary(
          Literal(Token.Literal.Number("1")),
          Token.Operator.Plus,
          Literal(Token.Literal.Number("2")),
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
            Token.Operator.Minus,
            Literal(Token.Literal.Number("123")),
          ),
          Token.Operator.Star,
          Grouping(Literal(Token.Literal.Number("45.67"))),
        )
      ),
      "(* (- 123) (group 45.67))",
    )
  }
