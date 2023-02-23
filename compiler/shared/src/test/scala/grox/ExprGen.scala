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

import Span.*

object ExprGen:
  type BinOperator = Expr => Expr => Expr

  type Term = Expr.Add | Expr.Subtract
  type Factor = Expr.Multiply | Expr.Divide
  type Equality = Expr.Equal | Expr.NotEqual

  def group[T: Typeable](
    expr: Expr
  ): Expr =
    expr match
      case _: T => Expr.Grouping(expr)
      case _    => expr

  def binaryGen(
    operator: BinOperator
  )(
    operand: => Gen[Expr]
  ): Gen[Expr] =
    for
      left <- operand
      right <- operand
    yield operator(left)(right)

  def treeGen(
    operator: BinOperator,
    operand: => Gen[Expr],
  ): Gen[Expr] = Gen.sized(size =>
    for
      left <- Gen.resize((size - 1) / 2, operand)
      right <- Gen.resize((size - 1) / 2, operand)
    yield operator(left)(right)
  )

  def addOperator(
    left: Expr,
    right: Expr,
  ): Expr = Expr.Add(empty, left, right)

  def subtractOperator(
    left: Expr,
    right: Expr,
  ): Expr =
    val gRight = group[Term](right)
    Expr.Subtract(empty, left, gRight)

  def multiplyOperator(
    left: Expr,
    right: Expr,
  ): Expr =
    val gLeft = group[Term](left)
    val gRight = group[Term](right)
    Expr.Multiply(empty, gLeft, gRight)

  def divideOperator(
    left: Expr,
    right: Expr,
  ): Expr =
    val gLeft = group[Term](left)
    val gRight = group[Term | Factor](right)
    Expr.Divide(empty, gLeft, gRight)

  def negateOperator(
    left: Expr
  ): Expr =
    left match
      case _: Expr.Literal => Expr.Negate(empty, left)
      case _               => Expr.Negate(empty, Expr.Grouping(left))

  val addGen = treeGen(addOperator.curried, numericGen)
  val subtractGen = treeGen(subtractOperator.curried, numericGen)
  val multiplyGen = treeGen(multiplyOperator.curried, numericGen)
  val divideGen = treeGen(divideOperator.curried, numericGen)

  val negateGen: Gen[Expr] = Gen.sized(size =>
    for expr <- Gen.resize(size - 1, numericGen)
    yield negateOperator(expr)
  )

  val numericGen: Gen[Expr] = Gen.sized(size =>
    if (size == 0)
      Gen.choose(0, 10).map(Expr.Literal(empty, _))
    else
      Gen.oneOf(addGen, subtractGen, multiplyGen, divideGen, negateGen)
  )

  def equalOperator(
    left: Expr,
    right: Expr,
  ): Expr =
    val gLeft = group[Equality](left)
    val gRight = group[Equality](right)
    Expr.Equal(empty, gLeft, gRight)

  def notEqualOperator(
    left: Expr,
    right: Expr,
  ): Expr =
    val gLeft = group[Equality](left)
    val gRight = group[Equality](right)
    Expr.NotEqual(empty, gLeft, gRight)

  def notOperator(
    left: Expr
  ): Expr =
    left match
      case _: Expr.Literal => Expr.Not(empty, left)
      case _               => Expr.Not(empty, Expr.Grouping(left))

  val equalityGen: Gen[Expr] = Gen.oneOf(
    treeGen(equalOperator.curried, numericGen),
    treeGen(equalOperator.curried, logicalGen),
    treeGen(notEqualOperator.curried, numericGen),
    treeGen(notEqualOperator.curried, logicalGen),
  )

  val comparisonGen: Gen[Expr] = Gen.oneOf(
    treeGen(Expr.Greater.apply.curried(empty), numericGen),
    treeGen(Expr.GreaterEqual.apply.curried(empty), numericGen),
    treeGen(Expr.Less.apply.curried(empty), numericGen),
    treeGen(Expr.LessEqual.apply.curried(empty), numericGen),
  )

  val notGen = Gen.sized(size =>
    for expr <- Gen.resize(size - 1, logicalGen)
    yield notOperator(expr)
  )

  val logicalGen: Gen[Expr] = Gen.sized(size =>
    if (size == 0)
      Gen.oneOf(Expr.Literal(empty, true), Expr.Literal(empty, false))
    else
      Gen.oneOf(equalityGen, comparisonGen, notGen)
  )
