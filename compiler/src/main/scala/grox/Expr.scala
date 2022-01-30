package grox

// Todo add support for:
// [ ] String literal
// [ ] String operators
// [ ] comparision operators with Order
// [ ] variables

type S = Double | Boolean

enum Expr[T]:

  // math operators
  case Plus(left: Expr[Double], right: Expr[Double]) extends Expr[Double]
  case Minus(left: Expr[Double], right: Expr[Double]) extends Expr[Double]
  case Times(left: Expr[Double], right: Expr[Double]) extends Expr[Double]
  case Divide(left: Expr[Double], right: Expr[Double]) extends Expr[Double]
  case Negate(expr: Expr[Double]) extends Expr[Double]

  // logic operators
  case Not(expr: Expr[Boolean]) extends Expr[Boolean]
  case And(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean]
  case Or(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean]

  // comparision operators
  case Greater(left: Expr[Double], right: Expr[Double]) extends Expr[Boolean]
  case GreaterEqual(left: Expr[Double], right: Expr[Double]) extends Expr[Boolean]
  case Less(left: Expr[Double], right: Expr[Double]) extends Expr[Boolean]
  case LessEqual(left: Expr[Double], right: Expr[Double]) extends Expr[Boolean]
  case Equal(left: Expr[T], right: Expr[T]) extends Expr[Boolean]
  case NotEqual(left: Expr[T], right: Expr[T]) extends Expr[Boolean]

  case Grouping(expr: Expr[T]) extends Expr[T]

  case Bool(value: Boolean) extends Expr[Boolean]
  case Num(value: Double) extends Expr[Double]

  // case BoolVal(name: String) extends Expr[Boolean]
  // case NumVal(name: String) extends Expr[Double]

object Expr {

  def eval[T](expr: Expr[T]): T =
    expr match {
      case Plus(left, right)         => eval(left) + eval(right)
      case Minus(left, right)        => eval(left) - eval(right)
      case Times(left, right)        => eval(left) * eval(right)
      case Divide(left, right)       => eval(left) / eval(right)
      case Negate(expr)              => -eval(expr)
      case Not(expr)                 => !eval(expr)
      case And(left, right)          => eval(left) && eval(right)
      case Or(left, right)           => eval(left) || eval(right)
      case Greater(left, right)      => eval(left) > eval(right)
      case GreaterEqual(left, right) => eval(left) >= eval(right)
      case Less(left, right)         => eval(left) < eval(right)
      case LessEqual(left, right)    => eval(left) <= eval(right)
      case Equal(left, right)        => eval(left) == eval(right)
      case NotEqual(left, right)     => eval(left) != eval(right)
      case Grouping(expr)            => eval(expr)
      case Bool(value)               => value
      case Num(value)                => value
    }

}
