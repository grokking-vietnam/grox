package grox

import munit.CatsEffectSuite

class ParserTest extends CatsEffectSuite {


  test("leftParen") {
    assertEquals(Parser.leftParen.parseAll("("), Right(Token.LeftParen))
  }

  test("rightParen") {
    assertEquals(Parser.rightParen.parseAll(")"), Right(Token.RightParen))
  }

  test("leftBrace") {
    assertEquals(Parser.leftBrace.parseAll("{"), Right(Token.LeftBrace))
  }

  test("rightBrace") {
    assertEquals(Parser.rightBrace.parseAll("}"), Right(Token.RightBrace))
  }

  test("comma") {
    assertEquals(Parser.comma.parseAll(","), Right(Token.Comma))
  }

  test("dot") {
    assertEquals(Parser.dot.parseAll("."), Right(Token.Dot))
  }

  test("minus") {
    assertEquals(Parser.minus.parseAll("-"), Right(Token.Minus))
  }

  test("plus") {
    assertEquals(Parser.plus.parseAll("+"), Right(Token.Plus))
  }

  test("semicolon") {
    assertEquals(Parser.semicolon.parseAll(";"), Right(Token.Semicolon))
  }

  test("slash") {
    assertEquals(Parser.slash.parseAll("/"), Right(Token.Slash))
  }

  test("bang") {
    assertEquals(Parser.bang.parseAll("!"), Right(Token.Bang))
  }

  test("bangEqual") {
    assertEquals(Parser.bangEqual.parseAll("!="), Right(Token.BangEqual))
  }

  test("equal") {
    assertEquals(Parser.equal.parseAll("="), Right(Token.Equal))
  }

  test("equalEqual") {
    assertEquals(Parser.equalEqual.parseAll("=="), Right(Token.EqualEqual))
  }

  test("greater") {
    assertEquals(Parser.greater.parseAll(">"), Right(Token.Greater))
  }

  test("greaterEqual") {
    assertEquals(Parser.greaterEqual.parseAll(">="), Right(Token.GreaterEqual))
  }

  test("less") {
    assertEquals(Parser.less.parseAll("<"), Right(Token.Less))
  }

  test("lessEqual") {
    assertEquals(Parser.lessEqual.parseAll("<="), Right(Token.LessEqual))
  }

  test("equalOrEqualEqual ==") {
    assertEquals(Parser.equalEqualOrElseEqual.parseAll("="), Right(Token.Equal))
  }

  test("equalOrEqualEqual =") {
    assertEquals(Parser.equalEqualOrElseEqual.parseAll("=="), Right(Token.EqualEqual))
  }

  test("bangEqualOrElseBang !=") {
    assertEquals(Parser.bangEqualOrElseBang.parseAll("!="), Right(Token.BangEqual))
  }

  test("bangEqualOrElseBang !") {
    assertEquals(Parser.bangEqualOrElseBang.parseAll("!"), Right(Token.Bang))
  }

  test("greaterEqualOrElseGreater >=") {
    assertEquals(Parser.greaterEqualOrElseGreater.parseAll(">="), Right(Token.GreaterEqual))
  }

  test("greaterEqualOrElseGreater >") {
    assertEquals(Parser.greaterEqualOrElseGreater.parseAll(">"), Right(Token.Greater))
  }

  test("lessEqualOrElseLess <=") {
    assertEquals(Parser.lessEqualOrElseLess.parseAll("<="), Right(Token.LessEqual))
  }

  test("lessEqualOrElseLess <") {
    assertEquals(Parser.lessEqualOrElseLess.parseAll("<"), Right(Token.Less))
  }

}

