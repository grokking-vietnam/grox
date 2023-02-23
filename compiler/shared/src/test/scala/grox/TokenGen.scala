package grox

import org.scalacheck.{Arbitrary, Gen, Prop}

import Token.*

object TokenGen:

  def nelString(
    g: Gen[Char]
  ) = Gen.nonEmptyListOf[Char](g).map(_.mkString)

  val identifierGen = nelString(Gen.alphaChar).map(Identifier(_, ()))
  val numberGen = nelString(Gen.numChar).map(Number(_, ()))
  val strGen = nelString(Gen.alphaChar).map(s => Str(s"\"$s\"", ()))

  val literalGen = Gen.oneOf(identifierGen, numberGen, strGen)

  val operatorGen = Gen.oneOf(operators.toSeq)

  val keywordGen = Gen.oneOf(keywords.toSeq)

  val singleLineCommentGen = Gen.alphaNumStr.map(s => SingleLine(s"//$s\n", ()))
  val blockCommentGen = Gen.alphaNumStr.map(s => Block(s"/*$s*/", ()))
  val commentGen = Gen.oneOf(singleLineCommentGen, blockCommentGen)

  val tokenGen =
    for
      t <- Gen.oneOf(literalGen, operatorGen, keywordGen, commentGen)
      w <- Gen.oneOf(" ", "\t", "\n", "\r")
    yield t.lexeme ++ w

  val tokensGen =
    for
      n <- Gen.choose(1, 1000)
      ts <- Gen.listOfN(n, tokenGen)
      if ts.size > 0
    yield ts
