package grox

import scala.reflect.Typeable

import org.scalacheck.{Arbitrary, Gen, Prop}

object ExprGen:
  type BinOperator = (Expr, Expr) => Expr

  type Term = Expr.Add | Expr.Subtract
  type Factor = Expr.Multiply | Expr.Divide

  def group[T: Typeable](expr: Expr): Expr =
    expr match
      case _: T => Expr.Grouping(expr)
      case _    => expr

  def binaryGen(operator: BinOperator)(operand: => Gen[Expr]): Gen[Expr] =
    for {
      left <- operand
      right <- operand
    } yield operator(left, right)

  def chainingGen(operator: BinOperator, operand: => Gen[Expr]): Gen[Expr] = Gen.sized(size =>
    for {
      left <- Gen.resize((size - 1) / 2, operand)
      right <- Gen.resize((size - 1) / 2, operand)
    } yield operator(left, right)
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

  def notOperator(left: Expr): Expr =
    left match
      case _: Expr.Literal => Expr.Negate(left)
      case _               => Expr.Negate(Expr.Grouping(left))

  val addGen = chainingGen(addOperator, numericGen)
  val subtractGen = chainingGen(subtractOperator, numericGen)
  val multiplyGen = chainingGen(multiplyOperator, numericGen)
  val divideGen = chainingGen(divideOperator, numericGen)

  val notGen: Gen[Expr] = Gen.sized(size =>
    for {
      expr <- Gen.resize(size - 1, numericGen)
    } yield notOperator(expr)
  )

  val numericGen: Gen[Expr] = Gen.sized(size =>
    if (size == 0)
      Gen.choose(0, 100).map(Expr.Literal(_))
    else
      Gen.oneOf(addGen, subtractGen, multiplyGen, divideGen, notGen)
  )

  val comparisonGen: Gen[Expr] =
    for {
      left <- numericGen
      right <- numericGen
      operator <- Gen.oneOf(
        Expr.Greater.apply,
        Expr.GreaterEqual.apply,
        Expr.Less.apply,
        Expr.LessEqual.apply,
      )
    } yield operator(left, right)

  val equalityGen: Gen[Expr] = Gen.oneOf(
    binaryGen(Expr.Equal.apply)(numericGen),
    binaryGen(Expr.NotEqual.apply)(numericGen),
    binaryGen(Expr.Equal.apply)(comparisonGen),
    binaryGen(Expr.NotEqual.apply)(comparisonGen),
  )

  val logicalGen: Gen[Expr] = equalityGen
