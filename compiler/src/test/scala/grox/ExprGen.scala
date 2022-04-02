package grox

import org.scalacheck.{Arbitrary, Prop, Gen}

object ExprGen {

  def zipOperator(ops: List[Operator], operands: List[List[Token]]): List[Token] =
    if ops == Nil || operands == Nil
    then Nil
    else (ops.head :: operands.head) ::: zipOperator(ops.tail, operands.tail)

  def makeStr(g: Gen[Char]) = Gen.nonEmptyListOf[Char](g).map(_.mkString)
  def makeList(g: Gen[Token]) = g.map(List(_))

  val numberGen = makeStr(Gen.numChar).map(Literal.Number(_))
  val strGen = makeStr(Gen.alphaChar).map(s => Literal.Str(s"\"$s\""))
  // TODO
  // val groupingGen = exprGen.map(List(Operator.LeftBrace) ::: _ ::: List(Operator.RightBrace))

  val primaryGen = Gen.oneOf(
    makeList(numberGen),
    makeList(strGen),
    makeList(Gen.oneOf(Keyword.True, Keyword.False)),
    makeList(Gen.oneOf(List(Keyword.Nil))),
    // groupingGen,
  )

  val unaryGen =
    for {
      ops <- Gen.listOfN(2, Gen.oneOf(Operator.Bang, Operator.Minus))
      primary <- primaryGen
    } yield (ops ::: primary)

  def binaryGen(descendant: Gen[List[Token]], operators: List[Operator]) =
    for {
      d <- descendant
      ops <- Gen.listOfN(2, Gen.oneOf(operators))
      ds <- Gen.listOfN(2, descendant)
    } yield d ::: zipOperator(ops, ds)

  val factorGen = binaryGen(unaryGen, List(Operator.Star, Operator.Slash))
  val termGen = binaryGen(factorGen, List(Operator.Plus, Operator.Minus))

  val comparisonGen = binaryGen(
    termGen,
    List(Operator.Greater, Operator.GreaterEqual, Operator.Less, Operator.LessEqual),
  )

  val equalityGen = binaryGen(
    comparisonGen,
    List(Operator.EqualEqual, Operator.BangEqual),
  )

  // generate a list of tokens representing a grammarly valid expression
  val exprGen: Gen[List[Token]] = equalityGen
}
