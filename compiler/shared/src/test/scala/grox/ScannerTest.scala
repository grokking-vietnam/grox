package grox

import cats.*
import cats.implicits.*
import cats.parse.Caret

import munit.CatsEffectSuite
import org.scalacheck.{Arbitrary, Gen}

import Token.*

class ScannerTest extends munit.FunSuite:

  test("whitespaces empty") {
    assertEquals(Scanner.whitespaces.parseAll(""), Right(()))
  }

  test("whitespaces") {
    assertEquals(Scanner.whitespaces.parseAll(" \t"), Right(()))
  }

  test("whitespaces") {
    assertEquals(Scanner.whitespaces.parseAll("  "), Right(()))
  }
  test("equalOrEqualEqual ==") {
    assertEquals(Scanner.equalEqualOrEqual.parseAll("="), Right(Equal(())))
  }

  test("equalOrEqualEqual =") {
    assertEquals(Scanner.equalEqualOrEqual.parseAll("=="), Right(EqualEqual(())))
  }

  test("bangEqualOrBang !=") {
    assertEquals(Scanner.bangEqualOrBang.parseAll("!="), Right(BangEqual(())))
  }

  test("bangEqualOrBang !") {
    assertEquals(Scanner.bangEqualOrBang.parseAll("!"), Right(Bang(())))
  }

  test("greaterEqualOrGreater >=") {
    assertEquals(Scanner.greaterEqualOrGreater.parseAll(">="), Right(GreaterEqual(())))
  }

  test("greaterEqualOrGreater >") {
    assertEquals(Scanner.greaterEqualOrGreater.parseAll(">"), Right(Greater(())))
  }

  test("lessEqualOrLess <=") {
    assertEquals(Scanner.lessEqualOrLess.parseAll("<="), Right(LessEqual(())))
  }

  test("lessEqualOrLess <") {
    assertEquals(Scanner.lessEqualOrLess.parseAll("<"), Right(Less(())))
  }

  test("single line comment") {
    val comment = "// this is a comment"
    assertEquals(Scanner.singleLineComment.parseAll(comment), Right(SingleLine(comment, ())))
  }

  test("empty block comment") {
    val comment = "/**/"
    assertEquals(Scanner.blockComment.parseAll(comment), Right(Block(comment, ())))
  }

  test("block comment") {
    val comment = "/* this is a block comment */"
    assertEquals(Scanner.blockComment.parseAll(comment), Right(Block(comment, ())))
  }

  test("nested block comment") {
    val comment = "/* this is a /*nested*/ block /*comment*/ */"
    assertEquals(Scanner.blockComment.parseAll(comment), Right(Block(comment, ())))
  }

  test("single line comment empty") {
    val comment = "//"
    assertEquals(Scanner.singleLineComment.parseAll(comment), Right(SingleLine(comment, ())))
  }

  test("singleLineComment orElse Slash /") {
    assertEquals(Scanner.commentOrSlash.parseAll("/"), Right(Slash(())))
  }

  test("singleLineComment orElse Slash //") {
    val comment = "// this is a comment"
    assertEquals(
      Scanner.commentOrSlash.parseAll(comment),
      Right(SingleLine(comment, ())),
    )
  }

  test("keywords") {
    keywords.foreach { keyword =>
      assertEquals(Scanner.keyword.parseAll(keyword.lexeme), Right(keyword))
    }
  }

  test("identifier") {
    val identifier = "orchi_1231"
    assertEquals(Scanner.identifier.parseAll(identifier), Right(Identifier(identifier, ())))
  }

  test("identifier _") {
    val identifier = "_"
    assertEquals(Scanner.identifier.parseAll(identifier), Right(Identifier(identifier, ())))
  }

  test("string") {
    val str = """"orchi_1231""""
    assertEquals(Scanner.str.parseAll(str), Right(Str("orchi_1231", ())))
  }

  test("number") {
    val str = "1234"
    assertEquals(Scanner.number.parseAll(str), Right(Number(str, ())))
  }

  test("fraction only failed") {
    val str = ".1234"
    assertEquals(Scanner.number.parseAll(str).isLeft, true)
  }

  test("number with frac") {
    val str = "1234.2323"
    assertEquals(Scanner.number.parseAll(str), Right(Number(str, ())))
  }

  test("number and dot failed") {
    val str = "1234."
    assertEquals(Scanner.number.parseAll(str).isLeft, true)
  }

  test("identifiers.lox simpler") {
    val str = "andy formless"
    val expected = List(
      Identifier("andy", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Identifier("formless", Span(Location(0, 5, 5), Location(0, 13, 13))),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("identifiers.lox") {
    val str = """andy formless fo _ _123 _abc ab123

abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"""
    val expected = List(
      Identifier("andy", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Identifier("formless", Span(Location(0, 5, 5), Location(0, 14, 14))),
      Identifier("fo", Span(Location(0, 14, 14), Location(0, 17, 17))),
      Identifier("_", Span(Location(0, 17, 17), Location(0, 19, 19))),
      Identifier("_123", Span(Location(0, 19, 19), Location(0, 24, 24))),
      Identifier("_abc", Span(Location(0, 24, 24), Location(0, 29, 29))),
      Identifier("ab123", Span(Location(0, 29, 29), Location(2, 0, 36))),
      Identifier(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_",
        Span(Location(2, 0, 36), Location(2, 63, 99)),
      ),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("keywords.lox") {
    val str = """and class else false for fun if nil or return super this true var while"""
    val expected = List(
      And(Span(Location(0, 0, 0), Location(0, 4, 4))),
      Class(Span(Location(0, 4, 4), Location(0, 10, 10))),
      Else(Span(Location(0, 10, 10), Location(0, 15, 15))),
      False(Span(Location(0, 15, 15), Location(0, 21, 21))),
      For(Span(Location(0, 21, 21), Location(0, 25, 25))),
      Fun(Span(Location(0, 25, 25), Location(0, 29, 29))),
      If(Span(Location(0, 29, 29), Location(0, 32, 32))),
      Null(Span(Location(0, 32, 32), Location(0, 36, 36))),
      Or(Span(Location(0, 36, 36), Location(0, 39, 39))),
      Return(Span(Location(0, 39, 39), Location(0, 46, 46))),
      Super(Span(Location(0, 46, 46), Location(0, 52, 52))),
      This(Span(Location(0, 52, 52), Location(0, 57, 57))),
      True(Span(Location(0, 57, 57), Location(0, 62, 62))),
      Var(Span(Location(0, 62, 62), Location(0, 66, 66))),
      While(Span(Location(0, 66, 66), Location(0, 71, 71))),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("numbers.lox") {
    val str = """123
  123.456
  .456
  123."""
    val expected = List(
      Number("123", Span(Location(0, 0, 0), Location(1, 2, 6))),
      Number("123.456", Span(Location(1, 2, 6), Location(2, 2, 16))),
      Dot(Span(Location(2, 2, 16), Location(2, 3, 17))),
      Number("456", Span(Location(2, 3, 17), Location(3, 2, 23))),
      Number("123", Span(Location(3, 2, 23), Location(3, 5, 26))),
      Dot(Span(Location(3, 5, 26), Location(3, 6, 27))),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("punctuators.lox") {
    val str = """(){};,+-*!===<=>=!=<>/."""
    val expected = List(
      LeftParen(Span(Location(0, 0, 0), Location(0, 1, 1))),
      RightParen(Span(Location(0, 1, 1), Location(0, 2, 2))),
      LeftBrace(Span(Location(0, 2, 2), Location(0, 3, 3))),
      RightBrace(Span(Location(0, 3, 3), Location(0, 4, 4))),
      Semicolon(Span(Location(0, 4, 4), Location(0, 5, 5))),
      Comma(Span(Location(0, 5, 5), Location(0, 6, 6))),
      Plus(Span(Location(0, 6, 6), Location(0, 7, 7))),
      Minus(Span(Location(0, 7, 7), Location(0, 8, 8))),
      Star(Span(Location(0, 8, 8), Location(0, 9, 9))),
      BangEqual(Span(Location(0, 9, 9), Location(0, 11, 11))),
      EqualEqual(Span(Location(0, 11, 11), Location(0, 13, 13))),
      LessEqual(Span(Location(0, 13, 13), Location(0, 15, 15))),
      GreaterEqual(Span(Location(0, 15, 15), Location(0, 17, 17))),
      BangEqual(Span(Location(0, 17, 17), Location(0, 19, 19))),
      Less(Span(Location(0, 19, 19), Location(0, 20, 20))),
      Greater(Span(Location(0, 20, 20), Location(0, 21, 21))),
      Slash(Span(Location(0, 21, 21), Location(0, 22, 22))),
      Dot(Span(Location(0, 22, 22), Location(0, 23, 23))),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("strings.lox") {
    val str = """""
  "string"
  """
    val expected = List(
      Str("", Span(Location(0, 0, 0), Location(1, 2, 5))),
      Str("string", Span(Location(1, 2, 5), Location(2, 2, 16))),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("spaces.lox") {
    val str = """
  space    tabs				newlines

  end

  """
    val expected = List(
      Identifier("space", Span(Location(0, 0, 0), Location(1, 11, 12))),
      Identifier("tabs", Span(Location(1, 11, 12), Location(1, 19, 20))),
      Identifier("newlines", Span(Location(1, 19, 20), Location(3, 2, 32))),
      Identifier("end", Span(Location(3, 2, 32), Location(5, 2, 39))),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("multiline.lox") {
    val str = """
  var a = "1
  2
  3";
  print a;
  // expect: 1
  // expect: 2
  // expect: 3
  """
    val expected = List(
      Var(Span(Location(0, 0, 0), Location(1, 6, 7))),
      Identifier("a", Span(Location(1, 6, 7), Location(1, 8, 9))),
      Equal(Span(Location(1, 8, 9), Location(1, 10, 11))),
      Str(
        """1
  2
  3""",
        Span(Location(1, 10, 11), Location(3, 4, 22)),
      ),
      Semicolon(Span(Location(3, 4, 22), Location(4, 2, 26))),
      Print(Span(Location(4, 2, 26), Location(4, 8, 32))),
      Identifier("a", Span(Location(4, 8, 32), Location(4, 9, 33))),
      Semicolon(Span(Location(4, 9, 33), Location(5, 2, 37))),
      SingleLine("// expect: 1", Span(Location(5, 2, 37), Location(6, 2, 52))),
      SingleLine("// expect: 2", Span(Location(6, 2, 52), Location(7, 2, 67))),
      SingleLine("// expect: 3", Span(Location(7, 2, 67), Location(8, 2, 82))),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("return_in_nested_function.lox") {
    val str = """
  class Foo {
  init() {
  fun init() {
  return "bar";
  }
  print init(); // expect: bar
  }
  }

  print Foo(); // expect: Foo instance"""
    val expected = List(
      Class(Span(Location(0, 0, 0), Location(1, 8, 9))),
      Identifier("Foo", Span(Location(1, 8, 9), Location(1, 12, 13))),
      LeftBrace(Span(Location(1, 12, 13), Location(2, 2, 17))),
      Identifier("init", Span(Location(2, 2, 17), Location(2, 6, 21))),
      LeftParen(Span(Location(2, 6, 21), Location(2, 7, 22))),
      RightParen(Span(Location(2, 7, 22), Location(2, 9, 24))),
      LeftBrace(Span(Location(2, 9, 24), Location(3, 2, 28))),
      Fun(Span(Location(3, 2, 28), Location(3, 6, 32))),
      Identifier("init", Span(Location(3, 6, 32), Location(3, 10, 36))),
      LeftParen(Span(Location(3, 10, 36), Location(3, 11, 37))),
      RightParen(Span(Location(3, 11, 37), Location(3, 13, 39))),
      LeftBrace(Span(Location(3, 13, 39), Location(4, 2, 43))),
      Return(Span(Location(4, 2, 43), Location(4, 9, 50))),
      Str("bar", Span(Location(4, 9, 50), Location(4, 14, 55))),
      Semicolon(Span(Location(4, 14, 55), Location(5, 2, 59))),
      RightBrace(Span(Location(5, 2, 59), Location(6, 2, 63))),
      Print(Span(Location(6, 2, 63), Location(6, 8, 69))),
      Identifier("init", Span(Location(6, 8, 69), Location(6, 12, 73))),
      LeftParen(Span(Location(6, 12, 73), Location(6, 13, 74))),
      RightParen(Span(Location(6, 13, 74), Location(6, 14, 75))),
      Semicolon(Span(Location(6, 14, 75), Location(6, 16, 77))),
      SingleLine("// expect: bar", Span(Location(6, 16, 77), Location(7, 2, 94))),
      RightBrace(Span(Location(7, 2, 94), Location(8, 2, 98))),
      RightBrace(Span(Location(8, 2, 98), Location(10, 2, 103))),
      Print(Span(Location(10, 2, 103), Location(10, 8, 109))),
      Identifier("Foo", Span(Location(10, 8, 109), Location(10, 11, 112))),
      LeftParen(Span(Location(10, 11, 112), Location(10, 12, 113))),
      RightParen(Span(Location(10, 12, 113), Location(10, 13, 114))),
      Semicolon(Span(Location(10, 13, 114), Location(10, 15, 116))),
      SingleLine("// expect: Foo instance", Span(Location(10, 15, 116), Location(10, 38, 139))),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }
