package grox

import Expr.*

object AstPrinter:

  final def print(expr: Expr): String =
    expr match
      case Number(value) => value.toString
      case Str(value)    => value.toString
      case Negate(e)     => s"(! ${print(e)})"
      case Minus(e)      => s"(- ${print(e)})"
      case Grouping(e)   => s"(group ${print(e)})"

      // Binary
      case Add(left, right)      => s"(+ ${print(left)} ${print(right)})"
      case Subtract(left, right) => s"(- ${print(left)} ${print(right)})"
      case Multiply(left, right) => s"(* ${print(left)} ${print(right)})"
      case Divide(left, right)   => s"(/ ${print(left)} ${print(right)})"
