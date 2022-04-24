/*
Expression generation rules, which always result in a evaluable epxression:

numeric -> NUMBER | negate | binary
          | ( numeric )
negate  -> - numeric
binary  -> numeric (+ | - | * | /) numeric

logical     -> true | false
            | equality | comparison | not
            | ( logical )
equality    -> numeric ( == | != ) numeric
            / logical ( == | != ) logical
comparison  -> numeric ( > | >= | < | <= ) numeric
not         -> ! logical

 */
package grox

import scala.reflect.Typeable

import org.scalacheck.{Arbitrary, Gen, Prop}

object ExprGen:
  type BinOperator = (Expr, Expr) => Expr

  type Term = Expr.Add | Expr.Subtract
  type Factor = Expr.Multiply | Expr.Divide
  type Equality = Expr.Equal | Expr.NotEqual

  def group[T: Typeable](expr: Expr): Expr =
    expr match
      case _: T => Expr.Grouping(expr)
      case _    => expr

  def binaryGen(operator: BinOperator)(operand: => Gen[Expr]): Gen[Expr] =
    for
      left <- operand
      right <- operand
    yield operator(left, right)

  def treeGen(operator: BinOperator, operand: => Gen[Expr]): Gen[Expr] = Gen.sized(size =>
    for
      left <- Gen.resize((size - 1) / 2, operand)
      right <- Gen.resize((size - 1) / 2, operand)
    yield operator(left, right)
  )

  def addOperator(left: Expr, right: Expr): Expr = Expr.Add(left, right)

  def subtractOperator(left: Expr, right: Expr): Expr =
    val gRight = group[Term](right)
    Expr.Subtract(left, gRight)

  def multiplyOperator(left: Expr, right: Expr): Expr =
    val gLeft = group[Term](left)
    val gRight = group[Term](right)
    Expr.Multiply(gLeft, gRight)

  def divideOperator(left: Expr, right: Expr): Expr =
    val gLeft = group[Term](left)
    val gRight = group[Term | Factor](right)
    Expr.Divide(gLeft, gRight)

  def negateOperator(left: Expr): Expr =
    left match
      case _: Expr.Literal => Expr.Negate(left)
      case _               => Expr.Negate(Expr.Grouping(left))

  val addGen = treeGen(addOperator, numericGen)
  val subtractGen = treeGen(subtractOperator, numericGen)
  val multiplyGen = treeGen(multiplyOperator, numericGen)
  val divideGen = treeGen(divideOperator, numericGen)

  val negateGen: Gen[Expr] = Gen.sized(size =>
    for expr <- Gen.resize(size - 1, numericGen)
    yield negateOperator(expr)
  )

  val numericGen: Gen[Expr] = Gen.sized(size =>
    if (size == 0)
      Gen.choose(0, 100).map(Expr.Literal(_))
    else
      Gen.oneOf(addGen, subtractGen, multiplyGen, divideGen, negateGen)
  )

  def equalOperator(left: Expr, right: Expr): Expr =
    val gLeft = group[Equality](left)
    val gRight = group[Equality](right)
    Expr.Equal(gLeft, gRight)

  def notEqualOperator(left: Expr, right: Expr): Expr =
    val gLeft = group[Equality](left)
    val gRight = group[Equality](right)
    Expr.NotEqual(gLeft, gRight)

  def notOperator(left: Expr): Expr =
    left match
      case _: Expr.Literal => Expr.Not(left)
      case _               => Expr.Not(Expr.Grouping(left))

  val equalityGen: Gen[Expr] = Gen.oneOf(
    treeGen(equalOperator, numericGen),
    treeGen(equalOperator, logicalGen),
    treeGen(notEqualOperator, numericGen),
    treeGen(notEqualOperator, logicalGen),
  )

  val comparisonGen: Gen[Expr] = Gen.oneOf(
    treeGen(Expr.Greater.apply, numericGen),
    treeGen(Expr.GreaterEqual.apply, numericGen),
    treeGen(Expr.Less.apply, numericGen),
    treeGen(Expr.LessEqual.apply, numericGen),
  )

  val notGen = Gen.sized(size =>
    for expr <- Gen.resize(size - 1, logicalGen)
    yield notOperator(expr)
  )

  val logicalGen: Gen[Expr] = Gen.sized(size =>
    if (size == 0)
      Gen.oneOf(Expr.Literal(true), Expr.Literal(false))
    else
      Gen.oneOf(equalityGen, comparisonGen, notGen)
  )
