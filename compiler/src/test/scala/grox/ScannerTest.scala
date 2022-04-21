package grox

import cats.*
import cats.implicits.*

import munit.CatsEffectSuite
import org.scalacheck.{Arbitrary, Gen}
import cats.parse.Caret

class ScannerTest extends munit.FunSuite:

  import Operator.*

  def parseToLexemes(str: String) = Scanner.parse(str).map(_.map(_.lexeme).mkString)

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
    assertEquals(Scanner.equalEqualOrEqual.parseAll("=").map(_.isInstanceOf[Equal]), Right(true))
  }

  test("equalOrEqualEqual =") {
    assertEquals(Scanner.equalEqualOrEqual.parseAll("==").map(_.isInstanceOf[EqualEqual]), Right(true))
  }

  test("bangEqualOrBang !=") {
    assertEquals(Scanner.bangEqualOrBang.parseAll("!=").map(_.isInstanceOf[BangEqual]), Right(true))
  }

  test("bangEqualOrBang !") {
    assertEquals(Scanner.bangEqualOrBang.parseAll("!").map(_.isInstanceOf[Bang]), Right(true))
  }

  test("greaterEqualOrGreater >=") {
    assertEquals(Scanner.greaterEqualOrGreater.parseAll(">=").map(_.isInstanceOf[GreaterEqual]), Right(true))
  }

  test("greaterEqualOrGreater >") {
    assertEquals(Scanner.greaterEqualOrGreater.parseAll(">").map(_.isInstanceOf[Greater]), Right(true))
  }

  test("lessEqualOrLess <=") {
    assertEquals(Scanner.lessEqualOrLess.parseAll("<=").map(_.isInstanceOf[LessEqual]), Right(true))
  }

  test("lessEqualOrLess <") {
    assertEquals(Scanner.lessEqualOrLess.parseAll("<").map(_.isInstanceOf[Less]), Right(true))
  }

  test("single line comment") {
    val comment = "// this is a comment"
    assertEquals(Scanner.singleLineComment.parseAll(comment).map(_.asInstanceOf[Comment.SingleLine].lexeme), Right(comment))
  }

  test("single line comment empty") {
    val comment = "//"
    assertEquals(Scanner.singleLineComment.parseAll(comment).map(_.asInstanceOf[Comment.SingleLine].lexeme), Right(comment))
  }

  test("empty block comment") {
    val comment = "/**/"
    assertEquals(Scanner.blockComment.parseAll(comment).map(_.asInstanceOf[Comment.Block].lexeme), Right(comment))
  }

  test("block comment") {
    val comment = "/* this is a block comment */"
    assertEquals(Scanner.blockComment.parseAll(comment).map(_.asInstanceOf[Comment.Block].lexeme), Right(comment))
  }

  test("nested block comment") {
    val comment = "/* this is a /*nested*/ block /*comment*/ */"
    assertEquals(Scanner.blockComment.parseAll(comment).map(_.asInstanceOf[Comment.Block].lexeme), Right(comment))
  }

  test("singleLineComment orElse Slash /") {
    assertEquals(Scanner.commentOrSlash.parseAll("/").map(_.isInstanceOf[Slash]), Right(true))
  }

  test("singleLineComment orElse Slash //") {
    val comment = "// this is a comment"
    assertEquals(
      Scanner.commentOrSlash.parseAll(comment).map(_.asInstanceOf[Comment.SingleLine].lexeme),
      Right(comment),
    )
  }

  test("keywords") {
    // todo
    //Keyword.values.foreach { keyword =>
      //assertEquals(Scanner.keyword.parseAll(keyword.lexeme), Right(keyword))
    //}
  }

  test("identifier") {
    val identifier = "orchi_1231"
    assertEquals(Scanner.identifier.parseAll(identifier).map(_.asInstanceOf[Literal.Identifier].lexeme), Right(identifier))
  }

  test("identifier _") {
    val identifier = "_"
    assertEquals(Scanner.identifier.parseAll(identifier).map(_.asInstanceOf[Literal.Identifier].lexeme), Right(identifier))
  }

  test("string") {
    val str = """"orchi_1231""""
    assertEquals(Scanner.str.parseAll(str).map(_.asInstanceOf[Literal.Str].lexeme), Right("orchi_1231"))
  }

  test("number") {
    val str = "1234"
    assertEquals(Scanner.number.parseAll(str).map(_.asInstanceOf[Literal.Number].lexeme), Right(str))
  }

  test("fraction only failed") {
    val str = ".1234"
    assertEquals(Scanner.number.parseAll(str).isLeft, true)
  }

  test("number with frac") {
    val str = "1234.2323"
    assertEquals(Scanner.number.parseAll(str).map(_.asInstanceOf[Literal.Number].lexeme), Right(str))
  }

  test("number and dot failed") {
    val str = "1234."
    assertEquals(Scanner.number.parseAll(str).isLeft, true)
  }

  test("identifiers.lox simpler") {
    val str = "andy formless"
    val expected: List[Token] = List(
      Literal.Identifier("andy", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Identifier("formless", Span(Location(0, 5, 5), Location(0, 13, 13))),
    )
    assertEquals(Scanner.parse(str), Right(expected))
  }

  test("identifiers.lox") {
    val str = """andy formless fo _ _123 _abc ab123
abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_"""
    val expected: List[Token] = List(
      Literal.Identifier("andy", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Identifier("formless", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Identifier("fo", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Identifier("_", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Identifier("_123", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Identifier("_abc", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Identifier("ab123", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Identifier("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_", Span(Location(0, 0, 0), Location(0, 5, 5))),
    )
     assertEquals(Scanner.parse(str), Right(expected))
  }

  test("keywords.lox") {
    val str = """and class else false for fun if nil or return super this true var while"""
    val expected: List[Token] = List(
      Keyword.And(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.Class(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.Else(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.False(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.For(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.Fun(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.If(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.Nil(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.Or(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.Return(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.Super(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.This(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.True(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.Var(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Keyword.While(Span(Location(0, 0, 0), Location(0, 5, 5))),
    )
     assertEquals(Scanner.parse(str), Right(expected))
  }

  test("numbers.lox") {
    val str = """123
    123.456
      .456
      123."""
    val expected: List[Token] = List(
      Literal.Number("123", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Number("123.456", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Operator.Dot(Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Number("456", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Literal.Number("123", Span(Location(0, 0, 0), Location(0, 5, 5))),
      Operator.Dot(Span(Location(0, 0, 0), Location(0, 5, 5))),
    )
     assertEquals(Scanner.parse(str), Right(expected))
  }

  test("punctuators.lox") {
    val str = """(){};,+-*!===<=>=!=<>/."""
     assertEquals(parseToLexemes(str), Right(str))
  }


  //test("strings.lox") {
    //val str = """""
//"string"
      //"""
    //val expected: List[Token] = List(
      //Literal.Str("", Span(Location(0, 0 , 0), Location(0,0,0))),
      //Literal.Str("string", Span(Location(0, 0 , 0), Location(0,0,0))),
    //)
    //assertEquals(Scanner.parse(str), Right(str))
  //}

  //test("spaces.lox") {
    //val str = """
    //space    tabs				newlines




//end

//"""
     //assertEquals(parseToLexemes(str), Right("spacetabsnewlinesend"))
  //}

  //test("multiline.lox") {
    //val str = """
//var a = "1
//2
//3";
//print a;
//// expect: 1
//// expect: 2
//// expect: 3
    //"""
    //val expected: List[Token] = List(
      //Keyword.Var,
      //Literal.Identifier("a"),
      //Operator.Equal,
      //Literal.Str("""1
//2
//3"""),
      //Operator.Semicolon,
      //Keyword.Print,
      //Literal.Identifier("a"),
      //Operator.Semicolon,
      //Comment.SingleLine("// expect: 1"),
      //Comment.SingleLine("// expect: 2"),
      //Comment.SingleLine("// expect: 3"),
    //)
    //// assertEquals(Scanner.parse(str), Right(expected))
  //}

  //test("return_in_nested_function.lox") {
    //val str = """
//class Foo {
  //init() {
    //fun init() {
      //return "bar";
    //}
    //print init(); // expect: bar
  //}
//}

//print Foo(); // expect: Foo instance"""
    //val expected: List[Token] = List(
      //Keyword.Class,
      //Literal.Identifier("Foo"),
      //Operator.LeftBrace,
      //Literal.Identifier("init"),
      //Operator.LeftParen,
      //Operator.RightParen,
      //Operator.LeftBrace,
      //Keyword.Fun,
      //Literal.Identifier("init"),
      //Operator.LeftParen,
      //Operator.RightParen,
      //Operator.LeftBrace,
      //Keyword.Return,
      //Literal.Str("bar"),
      //Operator.Semicolon,
      //Operator.RightBrace,
      //Keyword.Print,
      //Literal.Identifier("init"),
      //Operator.LeftParen,
      //Operator.RightParen,
      //Operator.Semicolon,
      //Comment.SingleLine("// expect: bar"),
      //Operator.RightBrace,
      //Operator.RightBrace,
      //Keyword.Print,
      //Literal.Identifier("Foo"),
      //Operator.LeftParen,
      //Operator.RightParen,
      //Operator.Semicolon,
      //Comment.SingleLine("// expect: Foo instance"),
    //)
    //// assertEquals(Scanner.parse(str), Right(expected))
  //}
