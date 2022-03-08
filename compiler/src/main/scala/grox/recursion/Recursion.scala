package grox.recursion

import cats.Functor

import grox.Expr
import grox.Expr.*

def cata[F[_]: Functor, A, B](
  algebra: F[B] => B, // F-Algebra
  project: A => F[A],
): A => B = {

  def loop(state: A): B = algebra(Functor[F].map(project(state))(loop))

  loop
}

enum ExprF[+A]:
  case AddF(left: A, right: A)
  case SubtractF(left: A, right: A)
  case MultiplyF(left: A, right: A)
  case DivideF(left: A, right: A)

  case GreaterF(left: A, right: A)
  case GreaterEqualF(left: A, right: A)
  case LessF(left: A, right: A)
  case LessEqualF(left: A, right: A)
  case EqualF(left: A, right: A)
  case NotEqualF(left: A, right: A)

  case NegateF(expr: A)
  case NotF(expr: A)

  case LiteralF(value: LiteralType)
  case GroupingF(expr: A)

object Recursion:

  import ExprF.*

  given Functor[ExprF] with

    def map[A, B](expr: ExprF[A])(f: A => B): ExprF[B] =
      expr match
        case AddF(left, right)      => AddF(f(left), f(right))
        case SubtractF(left, right) => SubtractF(f(left), f(right))
        case MultiplyF(left, right) => MultiplyF(f(left), f(right))
        case DivideF(left, right)   => DivideF(f(left), f(right))

        case GreaterF(left, right)      => GreaterF(f(left), f(right))
        case GreaterEqualF(left, right) => GreaterEqualF(f(left), f(right))
        case LessF(left, right)         => LessF(f(left), f(right))
        case LessEqualF(left, right)    => LessEqualF(f(left), f(right))
        case EqualF(left, right)        => EqualF(f(left), f(right))
        case NotEqualF(left, right)     => NotEqualF(f(left), f(right))

        case NegateF(expr) => NegateF(f(expr))
        case NotF(expr)    => NotF(f(expr))

        case LiteralF(expr)  => LiteralF(expr)
        case GroupingF(expr) => GroupingF(f(expr))

    val project: Expr => ExprF[Expr] = {
      case Add(left, right)      => AddF(left, right)
      case Subtract(left, right) => SubtractF(left, right)
      case Multiply(left, right) => MultiplyF(left, right)
      case Divide(left, right)   => DivideF(left, right)

      // comparison
      case Greater(left, right)      => GreaterF(left, right)
      case GreaterEqual(left, right) => GreaterEqualF(left, right)
      case Less(left, right)         => LessF(left, right)
      case LessEqual(left, right)    => LessEqualF(left, right)
      case Equal(left, right)        => EqualF(left, right)
      case NotEqual(left, right)     => NotEqualF(left, right)

      // Unary
      case Negate(expr) => NegateF(expr)
      case Not(expr)    => NotF(expr)

      case Literal(value) => LiteralF(value)
      case Grouping(expr) => GroupingF(expr)
    }

    val mkStringAlgebra: ExprF[String] => String = {
      case AddF(left, right)      => s"$left + $right"
      case SubtractF(left, right) => s"$left - $right"
      case MultiplyF(left, right) => s"$left * $right"
      case DivideF(left, right)   => s"$left / $right"

      case GreaterF(left, right)      => s"$left > $right"
      case GreaterEqualF(left, right) => s"$left >= $right"
      case LessF(left, right)         => s"$left < $right"
      case LessEqualF(left, right)    => s"$left <= $right"
      case EqualF(left, right)        => s"$left == $right"
      case NotEqualF(left, right)     => s"$left != $right"

      case NegateF(expr) => s"-$expr"
      case NotF(expr)    => s"$expr"

      case LiteralF(expr)  => expr.toString
      case GroupingF(expr) => s"($expr)"
    }

end Recursion
