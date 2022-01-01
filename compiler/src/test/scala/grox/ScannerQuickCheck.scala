package grox

import scala.deriving.Mirror

import cats.*
import cats.implicits.*
import Token.*
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen, Prop}

class ScannerQuickCheck extends ScalaCheckSuite {

  // generate random list of tokens
  // and put them together as input
  // our sanner should parse it succesfully
  property("parse succesfully") {
    Prop.forAll(loxGen) { ts =>
      val lox = ts.foldMap(identity)
      Scanner.parse(lox).isRight == true
    }
  }

  // generate random list of tokens
  // and put them together as input
  // our sanner should parse it succesfully
  // and the result should have the same size as the input
  property("parsed tokens should have the same size as input") {
    Prop.forAll(loxGen) { ts =>
      val lox = ts.foldMap(identity)
      val resultSize = Scanner.parse(lox).map(_.size)
      resultSize == Right(ts.size)
    }
  }
  def nelString(g: Gen[Char]) = Gen.nonEmptyListOf[Char](g).map(_.mkString)
  val identifierGen = nelString(Gen.alphaChar).map(Literal.Identifier(_))
  val numberGen = nelString(Gen.numChar).map(Literal.Number(_))
  val strGen = nelString(Gen.alphaChar).map(s => Literal.Str(s"\"$s\""))

  val literalGen = Gen.oneOf(identifierGen, numberGen, strGen)

  val operatorGen = Gen.oneOf(Operator.values.toSeq)

  val keywordGen = Gen.oneOf(Keyword.values.toSeq)

  val singleLineCommentGen = Gen.alphaNumStr.map(s => Comment.SingleLine(s"//$s\n"))
  val blockCommentGen = Gen.alphaNumStr.map(s => Comment.Block(s"/*$s*/"))
  val commentGen = Gen.oneOf(singleLineCommentGen, blockCommentGen)

  val tokenGen =
    for {
      t <- Gen.oneOf(literalGen, operatorGen, keywordGen, commentGen)
      w <- Gen.oneOf(" ", "\t", "\n", "\r")
    } yield t.lexeme ++ w

  val loxGen =
    for {
      n <- Gen.choose(1, 1000)
      ts <- Gen.listOfN(n, tokenGen)
      if ts.size > 0
    } yield ts

}
