package grox

import cats.implicits.*
import cats.instances.*
import cats.kernel.{Eq, Order}

// Todo
// Array
// Nil
enum Expr[T: Order]:

  // math operators
  case Plus(left: Expr[Double], right: Expr[Double]) extends Expr[Double]
  case Minus(left: Expr[Double], right: Expr[Double]) extends Expr[Double]
  case Times(left: Expr[Double], right: Expr[Double]) extends Expr[Double]
  case Divide(left: Expr[Double], right: Expr[Double]) extends Expr[Double]
  case Negate(exp: Expr[Double]) extends Expr[Double]

  // logic operator
  case Not(exp: Expr[Boolean]) extends Expr[Boolean]

  // order operator
  case Greater[T: Order](left: Expr[T], right: Expr[T]) extends Expr[Boolean]
  case GreaterEqual[T: Order](left: Expr[T], right: Expr[T]) extends Expr[Boolean]
  case Less[T: Order](left: Expr[T], right: Expr[T]) extends Expr[Boolean]
  case LessEqual[T: Order](left: Expr[T], right: Expr[T]) extends Expr[Boolean]

  case Equal[T: Eq](left: Expr[T], right: Expr[T]) extends Expr[Boolean]
  case NotEqual[T: Eq](left: Expr[T], right: Expr[T]) extends Expr[Boolean]

  case Bool(value: Boolean) extends Expr[Boolean]
  case Num(value: Double) extends Expr[Double]
  case Str(value: String) extends Expr[String]

  case BoolVal(name: String) extends Expr[Boolean]
  case NumVal(name: String) extends Expr[Double]
  case StrVal(name: String) extends Expr[String]

// string ++ Boolean => error
//case Concat(left: Exp[String], right: Exp[String]) extends Exp[String]

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

  def tEq[T: Eq](e: Equal[T]): Boolean = eval(e.left) === eval(e.right)

  def tAdd(e: Plus): Double = eval(e.left) + eval(e.right)

  def eval[T](expr: Expr[T]): T = ???
  // expr match {
  // case Add(left, right)     => eval(left) + eval(right)
  // case Greater(left, right) => eval(left) > eval(right)
  // case Bool(b)              => b
  // case Negate(e)            => -eval(expr)
  // case Numb(d)            => d
  // case Concat(l, r)         => eval(l) ++ eval(r)
  // }

  // def example() = {
  // val expr = Add(Number(3), Number(4))
  // val result: Double = eval(expr)
  // }

}
