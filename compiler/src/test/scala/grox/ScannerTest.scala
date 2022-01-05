package grox

import scala.deriving.Mirror

import cats.*
import cats.implicits.*

import munit.CatsEffectSuite
import org.scalacheck.{Arbitrary, Gen}

class ScannerTest extends munit.FunSuite {

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
    assertEquals(Scanner.equalEqualOrEqual.parseAll("="), Right(Operator.Equal))
  }

  test("equalOrEqualEqual =") {
    assertEquals(Scanner.equalEqualOrEqual.parseAll("=="), Right(Operator.EqualEqual))
  }

  test("bangEqualOrBang !=") {
    assertEquals(Scanner.bangEqualOrBang.parseAll("!="), Right(Operator.BangEqual))
  }

  test("bangEqualOrBang !") {
    assertEquals(Scanner.bangEqualOrBang.parseAll("!"), Right(Operator.Bang))
  }

  test("greaterEqualOrGreater >=") {
    assertEquals(Scanner.greaterEqualOrGreater.parseAll(">="), Right(Operator.GreaterEqual))
  }

  test("greaterEqualOrGreater >") {
    assertEquals(Scanner.greaterEqualOrGreater.parseAll(">"), Right(Operator.Greater))
  }

  test("lessEqualOrLess <=") {
    assertEquals(Scanner.lessEqualOrLess.parseAll("<="), Right(Operator.LessEqual))
  }

  test("lessEqualOrLess <") {
    assertEquals(Scanner.lessEqualOrLess.parseAll("<"), Right(Operator.Less))
  }

  test("single line comment") {
    val comment = "// this is a comment"
    assertEquals(Scanner.singleLineComment.parseAll(comment), Right((Comment.SingleLine(comment))))
  }

  test("empty block comment") {
    val comment = "/**/"
    assertEquals(Scanner.blockComment.parseAll(comment), Right((Comment.Block(comment))))
  }

  test("block comment") {
    val comment = "/* this is a block comment */"
    assertEquals(Scanner.blockComment.parseAll(comment), Right((Comment.Block(comment))))
  }

  test("nested block comment") {
    val comment = "/* this is a /*nested*/ block /*comment*/ */"
    assertEquals(Scanner.blockComment.parseAll(comment), Right(Comment.Block(comment)))
  }

  test("single line comment empty") {
    val comment = "//"
    assertEquals(Scanner.singleLineComment.parseAll(comment), Right((Comment.SingleLine(comment))))
  }

  test("singleLineComment orElse Slash /") {
    assertEquals(Scanner.commentOrSlash.parseAll("/"), Right((Operator.Slash)))
  }

  test("singleLineComment orElse Slash //") {
    val comment = "// this is a comment"
    assertEquals(
      Scanner.commentOrSlash.parseAll(comment),
      Right((Comment.SingleLine(comment))),
    )
  }

  test("keywords") {
    Keyword.values.foreach { keyword =>
      assertEquals(Scanner.keyword.parseAll(keyword.lexeme), Right(keyword))
    }
  }

  test("identifier") {
    val identifier = "orchi_1231"
    assertEquals(Scanner.identifier.parseAll(identifier), Right(Literal.Identifier(identifier)))
  }

  test("identifier _") {
    val identifier = "_"
    assertEquals(Scanner.identifier.parseAll(identifier), Right(Literal.Identifier(identifier)))
  }

  test("string") {
    val str = """"orchi_1231""""
    assertEquals(Scanner.str.parseAll(str), Right(Literal.Str("orchi_1231")))
  }

  test("number") {
    val str = "1234"
    assertEquals(Scanner.number.parseAll(str), Right(Literal.Number(str)))
  }

  test("fraction only failed") {
    val str = ".1234"
    assertEquals(Scanner.number.parseAll(str).isLeft, true)
  }

  test("number with frac") {
    val str = "1234.2323"
    assertEquals(Scanner.number.parseAll(str), Right(Literal.Number(str)))
  }

  test("number and dot failed") {
    val str = "1234."
    assertEquals(Scanner.number.parseAll(str).isLeft, true)
  }

  test("identifiers.lox simpler") {
    val str = "andy formless"
    val expected: List[Token] = List(
      Literal.Identifier("andy"),
      Literal.Identifier("formless"),
    )
    assertEquals(Scanner.parse(str), Right(expected))
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
      Literal.Identifier("ab123"),
      Literal.Identifier("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("keywords.lox") {
    val str = """and class else false for fun if nil or return super this true var while"""
    val expected: List[Token] = List(
      Keyword.And,
      Keyword.Class,
      Keyword.Else,
      Keyword.False,
      Keyword.For,
      Keyword.Fun,
      Keyword.If,
      Keyword.Nil,
      Keyword.Or,
      Keyword.Return,
      Keyword.Super,
      Keyword.This,
      Keyword.True,
      Keyword.Var,
      Keyword.While,
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("numbers.lox") {
    val str = """123
    123.456
      .456
      123."""
    val expected: List[Token] = List(
      Literal.Number("123"),
      Literal.Number("123.456"),
      Operator.Dot,
      Literal.Number("456"),
      Literal.Number("123"),
      Operator.Dot,
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("punctuators.lox") {
    val str = """(){};,+-*!===<=>=!=<>/."""
    val expected: List[Token] = List(
      Operator.LeftParen,
      Operator.RightParen,
      Operator.LeftBrace,
      Operator.RightBrace,
      Operator.Semicolon,
      Operator.Comma,
      Operator.Plus,
      Operator.Minus,
      Operator.Star,
      Operator.BangEqual,
      Operator.EqualEqual,
      Operator.LessEqual,
      Operator.GreaterEqual,
      Operator.BangEqual,
      Operator.Less,
      Operator.Greater,
      Operator.Slash,
      Operator.Dot,
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("strings.lox") {
    val str = """""
"string"
      """
    val expected: List[Token] = List(
      Literal.Str(""),
      Literal.Str("string"),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("spaces.lox") {
    val str = """
    space    tabs				newlines




end

"""
    val expected: List[Token] = List(
      Literal.Identifier("space"),
      Literal.Identifier("tabs"),
      Literal.Identifier("newlines"),
      Literal.Identifier("end"),
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
    val expected: List[Token] = List(
      Keyword.Var,
      Literal.Identifier("a"),
      Operator.Equal,
      Literal.Str("""1
2
3"""),
      Operator.Semicolon,
      Keyword.Print,
      Literal.Identifier("a"),
      Operator.Semicolon,
      Comment.SingleLine("// expect: 1"),
      Comment.SingleLine("// expect: 2"),
      Comment.SingleLine("// expect: 3"),
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
    val expected: List[Token] = List(
      Keyword.Class,
      Literal.Identifier("Foo"),
      Operator.LeftBrace,
      Literal.Identifier("init"),
      Operator.LeftParen,
      Operator.RightParen,
      Operator.LeftBrace,
      Keyword.Fun,
      Literal.Identifier("init"),
      Operator.LeftParen,
      Operator.RightParen,
      Operator.LeftBrace,
      Keyword.Return,
      Literal.Str("bar"),
      Operator.Semicolon,
      Operator.RightBrace,
      Keyword.Print,
      Literal.Identifier("init"),
      Operator.LeftParen,
      Operator.RightParen,
      Operator.Semicolon,
      Comment.SingleLine("// expect: bar"),
      Operator.RightBrace,
      Operator.RightBrace,
      Keyword.Print,
      Literal.Identifier("Foo"),
      Operator.LeftParen,
      Operator.RightParen,
      Operator.Semicolon,
      Comment.SingleLine("// expect: Foo instance"),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

}
