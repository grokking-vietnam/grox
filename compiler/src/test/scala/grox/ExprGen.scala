package grox

import scala.reflect.{Typeable, TypeTest}
import org.scalacheck.{Arbitrary, Prop, Gen}

object ExprGen {
  type BinOperator = (Expr, Expr) => Expr

  type Term = Expr.Add | Expr.Subtract
  type Factor = Expr.Multiply | Expr.Divide

  def group[T: Typeable](expr: Expr): Expr =
    expr match
      case _: T => Expr.Grouping(expr)
      case _    => expr

  def binaryGen(op: BinOperator): Gen[Expr] = Gen.sized(size =>
    for {
      left <- Gen.resize((size - 1) / 2, numericGen)
      right <- Gen.resize((size - 1) / 2, numericGen)
    } yield op(left, right)
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

  val addGen = binaryGen(addOperator)
  val subtractGen = binaryGen(subtractOperator)
  val multiplyGen = binaryGen(multiplyOperator)
  val divideGen = binaryGen(divideOperator)

  val notGen: Gen[Expr] = Gen.sized(size =>
    for {
      expr <- Gen.resize(size - 1, numericGen)
    } yield notOperator(expr)
  )

  val numericGen: Gen[Expr] = Gen.sized(size =>
    if (size == 0)
      Gen.choose(0, 100).map(Expr.Literal(_))
    else
      Gen.frequency((3, addGen), (3, subtractGen), (2, multiplyGen), (2, divideGen), (1, notGen))
  )

  // val logicalGen: Gen[Expr] = ???
}
