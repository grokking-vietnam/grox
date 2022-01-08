package grox

import cats.kernel.Comparison.GreaterThan

// Todo
// Array
// Nil
enum Expr[T]:
  case Add(left: Expr[Double], right: Expr[Double]) extends Expr[Double]

  // >
  case Greater(left: Expr[Double], right: Expr[Double]) extends Expr[Boolean]

  case Bool(value: Boolean) extends Expr[Boolean]
  case Number(value: Double) extends Expr[Double]

  case Negate(expr: Expr[Double]) extends Expr[Double]

  // string ++ Boolean => error
  case Concat(left: Expr[String], right: Expr[String]) extends Expr[String]

// val a = 3.0 => DoubleVariable()
// val a = 3.0 <= 4 => BoolVariable
// val a = 3.0 <= True => Error
// a = True => error
//case DoubleVariable(value: String) extends Expr[Double]

//case BoolVariable(value: String) extends Expr[Boolean]

//case Subtract(left: Expr, right: Expr)
//case Multiply(left: Expr, right: Expr)
//case Divide(left: Expr, right: Expr)
//case Negate(expr: Expr)
//case Minus(expr: Expr)
//case Boolean(value: Boolean)
//case Str(value: String)
//case Grouping(expr: Expr)

object Expr {

  def eval[T](expr: Expr[T]): T =
    expr match {
      case Add(left, right)     => eval(left) + eval(right)
      case Greater(left, right) => eval(left) > eval(right)
      case Bool(b)              => b
      case Negate(e)            => -eval(expr)
      case Number(d)            => d
      case Concat(l, r)         => eval(l) ++ eval(r)
    }

  def example() = {
    val expr = Add(Number(3), Number(4))
    val result: Double = eval(expr)
  }

}
