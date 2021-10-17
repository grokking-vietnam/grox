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
    assertEquals(Parser.equalEqualOrEqual.parseAll("="), Right(Operator.Equal))
  }

  test("equalOrEqualEqual =") {
    assertEquals(Parser.equalEqualOrEqual.parseAll("=="), Right(Operator.EqualEqual))
  }

  test("bangEqualOrBang !=") {
    assertEquals(Parser.bangEqualOrBang.parseAll("!="), Right(Operator.BangEqual))
  }

  test("bangEqualOrBang !") {
    assertEquals(Parser.bangEqualOrBang.parseAll("!"), Right(Operator.Bang))
  }

  test("greaterEqualOrGreater >=") {
    assertEquals(Parser.greaterEqualOrGreater.parseAll(">="), Right(Operator.GreaterEqual))
  }

  test("greaterEqualOrGreater >") {
    assertEquals(Parser.greaterEqualOrGreater.parseAll(">"), Right(Operator.Greater))
  }

  test("lessEqualOrLess <=") {
    assertEquals(Parser.lessEqualOrLess.parseAll("<="), Right(Operator.LessEqual))
  }

  test("lessEqualOrLess <") {
    assertEquals(Parser.lessEqualOrLess.parseAll("<"), Right(Operator.Less))
  }

  test("single line comment") {
    val comment = "// this is a comment"
    assertEquals(Parser.singleLineComment.parseAll(comment), Right((Comment.SingleLine(comment))))
  }

  test("single line comment empty") {
    val comment = "//"
    assertEquals(Parser.singleLineComment.parseAll(comment), Right((Comment.SingleLine(comment))))
  }

  test("singleLineComment orElse Slash /") {
    assertEquals(Parser.singleLineCommentOrSlash.parseAll("/"), Right((Operator.Slash)))
  }

  test("singleLineComment orElse Slash //") {
    val comment = "// this is a comment"
    assertEquals(
      Parser.singleLineCommentOrSlash.parseAll(comment),
      Right((Comment.SingleLine(comment))),
    )
  }

  test("keywords") {
    Keyword.values.foreach { keyword =>
      assertEquals(Parser.keyword.parseAll(keyword.lexeme ++ " "), Right(keyword))
    }
  }

  test("identifier") {
    val identifier = "orchi_1231"
    assertEquals(Parser.identifier.parseAll(identifier), Right(Literal.Identifier(identifier)))
  }

  test("identifier _") {
    val identifier = "_"
    assertEquals(Parser.identifier.parseAll(identifier), Right(Literal.Identifier(identifier)))
  }

  test("str") {
    val str = """"orchi_1231""""
    assertEquals(Parser.str.parseAll(str), Right(Literal.Str("orchi_1231")))
  }

  test("number") {
    val str = "1234"
    assertEquals(Parser.number.parseAll(str), Right(Literal.Number(str)))
  }

  test("number with frac") {
    val str = "1234.2323"
    assertEquals(Parser.number.parseAll(str), Right(Literal.Number(str)))
  }

  test("number fails") {
    val str = "1234."
    assertEquals(Parser.number.parseAll(str).isLeft, true)
  }

  test("identifiers.lox only") {
    val str = "andy formless "
    val expected: List[Token] = List(
      Literal.Identifier("andy"),
      Literal.Identifier("formless"),
    )
    val is: cats.parse.Parser[List[Token]] = (Parser.identifier <* Parser.whitespaces).rep.map(_.toList)
    assertEquals(is.parseAll(str), Right(expected))
  }

  test("identifiers.lox simpler") {
    val str = "andy formless"
    val expected: List[Token] = List(
      Literal.Identifier("andy"),
      Literal.Identifier("formless"),
    )
    assertEquals(Parser.parse(str), Right(expected))
  }

  test("identifiers.lox") {
    val str = """andy formless fo _ _123 _abc ab123
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"""
    val expected: List[Token] = List(
      Literal.Identifier("andy"),
      Literal.Identifier("formless"),
      Literal.Identifier("fo"),
      Literal.Identifier("_"),
      Literal.Identifier("_123"),
      Literal.Identifier("_abc"),
      Literal.Identifier("abc123"),
      Literal.Identifier("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"),
    )
    assertEquals(Parser.parse(str), Right(expected))
  }

  //test("identifierOrKeyword") {
  //val identifier = "orchi_1231"
  //assertEquals(Parser.identifierOrKeyword.parseAll(identifier), Right(Literal.Identifier(identifier)))
  //}

  //test("identifierOrKeyword or") {
  //val identifier = "or "
  //assertEquals(Parser.identifierOrKeyword.parseAll(identifier), Right(Keyword.Or))
  //}

}

