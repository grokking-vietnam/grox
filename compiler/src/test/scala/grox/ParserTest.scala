package grox

import munit.CatsEffectSuite

class ParserTest extends CatsEffectSuite {


  test("leftParen") {
    assertEquals(Parser.leftParen.parseAll("("), Right(Operator.LeftParen))
  }

  test("rightParen") {
    assertEquals(Parser.rightParen.parseAll(")"), Right(Operator.RightParen))
  }

  test("leftBrace") {
    assertEquals(Parser.leftBrace.parseAll("{"), Right(Operator.LeftBrace))
  }

  test("rightBrace") {
    assertEquals(Parser.rightBrace.parseAll("}"), Right(Operator.RightBrace))
  }

  test("comma") {
    assertEquals(Parser.comma.parseAll(","), Right(Operator.Comma))
  }

  test("dot") {
    assertEquals(Parser.dot.parseAll("."), Right(Operator.Dot))
  }

  test("minus") {
    assertEquals(Parser.minus.parseAll("-"), Right(Operator.Minus))
  }

  test("plus") {
    assertEquals(Parser.plus.parseAll("+"), Right(Operator.Plus))
  }

  test("semicolon") {
    assertEquals(Parser.semicolon.parseAll(";"), Right(Operator.Semicolon))
  }

  test("slash") {
    assertEquals(Parser.slash.parseAll("/"), Right(Operator.Slash))
  }

  test("bang") {
    assertEquals(Parser.bang.parseAll("!"), Right(Operator.Bang))
  }

  test("bangEqual") {
    assertEquals(Parser.bangEqual.parseAll("!="), Right(Operator.BangEqual))
  }

  test("equal") {
    assertEquals(Parser.equal.parseAll("="), Right(Operator.Equal))
  }

  test("equalEqual") {
    assertEquals(Parser.equalEqual.parseAll("=="), Right(Operator.EqualEqual))
  }

  test("greater") {
    assertEquals(Parser.greater.parseAll(">"), Right(Operator.Greater))
  }

  test("greaterEqual") {
    assertEquals(Parser.greaterEqual.parseAll(">="), Right(Operator.GreaterEqual))
  }

  test("less") {
    assertEquals(Parser.less.parseAll("<"), Right(Operator.Less))
  }

  test("lessEqual") {
    assertEquals(Parser.lessEqual.parseAll("<="), Right(Operator.LessEqual))
  }

  test("equalOrEqualEqual ==") {
    assertEquals(Parser.equalEqualOrElseEqual.parseAll("="), Right(Operator.Equal))
  }

  test("equalOrEqualEqual =") {
    assertEquals(Parser.equalEqualOrElseEqual.parseAll("=="), Right(Operator.EqualEqual))
  }

  test("bangEqualOrElseBang !=") {
    assertEquals(Parser.bangEqualOrElseBang.parseAll("!="), Right(Operator.BangEqual))
  }

  test("bangEqualOrElseBang !") {
    assertEquals(Parser.bangEqualOrElseBang.parseAll("!"), Right(Operator.Bang))
  }

  test("greaterEqualOrElseGreater >=") {
    assertEquals(Parser.greaterEqualOrElseGreater.parseAll(">="), Right(Operator.GreaterEqual))
  }

  test("greaterEqualOrElseGreater >") {
    assertEquals(Parser.greaterEqualOrElseGreater.parseAll(">"), Right(Operator.Greater))
  }

  test("lessEqualOrElseLess <=") {
    assertEquals(Parser.lessEqualOrElseLess.parseAll("<="), Right(Operator.LessEqual))
  }

  test("lessEqualOrElseLess <") {
    assertEquals(Parser.lessEqualOrElseLess.parseAll("<"), Right(Operator.Less))
  }

  test("single line comment") {
    val comment = "// this is a comment"
    assertEquals(Parser.singleLineComment.parseAll(comment), Right((Comment.SingleLine(comment))))
  }

  test("singleLineComment orElse Slash /") {
    assertEquals(Parser.singleLineCommentOrElseSlash.parseAll("/"), Right((Operator.Slash)))
  }

  test("singleLineComment orElse Slash //") {
    val comment = "// this is a comment"
    assertEquals(Parser.singleLineCommentOrElseSlash.parseAll(comment), Right((Comment.SingleLine(comment))))
  }

  test("keywords") {
    Keyword.values.foreach { keyword =>
      assertEquals(Parser.keyword.parseAll(keyword.lexeme), Right(keyword))
    }
  }

  test("identifier") {
    val identifier = "orchi_1231"
    assertEquals(Parser.identifier.parseAll(identifier), Right(Literal.Identifier(identifier)))
  }

}

