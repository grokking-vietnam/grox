package grox

import cats.*
import cats.implicits.*

import munit.ScalaCheckSuite
import org.scalacheck.Prop

class ScannerQuickCheck extends ScalaCheckSuite:

  // generate random list of tokens
  // and put them together as input
  // our sanner should parse it succesfully
  property("parse succesfully"):
    Prop.forAll(TokenGen.tokensGen) { ts =>
      val lox = ts.foldMap(identity)
      Scanner.parse(lox).isRight == true
    }

  // generate random list of tokens
  // and put them together as input
  // our sanner should parse it succesfully
  // and the result should have the same size as the input
  property("parsed tokens should have the same size as input"):
    Prop.forAll(TokenGen.tokensGen) { ts =>
      val lox = ts.foldMap(identity)
      val resultSize = Scanner.parse(lox).map(_.size)
      resultSize == Right(ts.size)
    }
