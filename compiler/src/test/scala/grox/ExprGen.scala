package grox

import org.scalacheck.{Arbitrary, Prop, Gen}

object ExprGen {
  // N is the maximum number of operators in a grammar rule.
  final val N = 4;

  val numberGen = mkString(Gen.numChar).map(Literal.Number(_))
  val strGen = mkString(Gen.alphaChar).map(s => Literal.Str(s"\"$s\""))
  // TODO
  // lazy val groupingGen = exprGen.map(List(Operator.LeftBrace) ::: _ ::: List(Operator.RightBrace))

  val primaryGen = Gen.oneOf(
    mkList(numberGen),
    mkList(strGen),
    mkList(Gen.oneOf(Keyword.True, Keyword.False)),
    mkList(Gen.oneOf(List(Keyword.Nil))),
    // groupingGen,
  )

  val unaryGen =
    for {
      ops <- Gen.listOfN(N, Gen.oneOf(Operator.Bang, Operator.Minus))
      primary <- primaryGen
    } yield (ops ::: primary)

  // generate up to N pairs of (operator, operand) which stands on the right side of a binary operation,
  // and return them as a flat list. e.g: (+, 3, -, 4)
  def rightSideGen(operandGen: Gen[List[Token]], operators: List[Operator]) =
    val opGen =
      for {
        operator <- Gen.oneOf(operators)
        operand <- operandGen
      } yield (operator, operand)

    def addTokens(tokens: List[Token], p: (Operator, List[Token])) =
      p match
        case (operator, operand) => tokens ::: operator :: operand

    for {
      rightSide <- Gen.listOfN(N, opGen)
    } yield rightSide.foldLeft(List[Token]())(addTokens)

  def binaryGen(operandGen: Gen[List[Token]], operators: List[Operator]) =
    for {
      left <- operandGen
      rightSide <- rightSideGen(operandGen, operators)
    } yield left ::: rightSide

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

  def mkString(g: Gen[Char]) = Gen.nonEmptyListOf[Char](g).map(_.mkString)
  def mkList(g: Gen[Token]) = g.map(List(_))

}
