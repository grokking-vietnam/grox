package grox

import cats.*
import cats.syntax.all.*

import kantan.parsers.Parser as P

trait ExprParser[F[_]]:
  def parse(tokens: List[Token[Span]]): F[Expr]

object ExprParser:
  import Token.*
  import TokenParser.{given, *}

  def instance[F[_]: MonadThrow]: ExprParser[F] = new:
    def parse(tokens: List[Token[Span]]): F[Expr] = run(expr)(tokens).liftTo[F]
  type BinaryOp = Expr => Expr => Expr
  type UnaryOp = Expr => Expr

  val literal = token.collect:
    case Number(l, tag)        => Expr.Literal(tag, l.toDouble)
    case Str(l, tag)           => Expr.Literal(tag, l)
    case True(tag)             => Expr.Literal(tag, true)
    case False(tag)            => Expr.Literal(tag, false)
    case Null(tag)             => Expr.Literal(tag, ())
    case Identifier(name, tag) => Expr.Variable(tag, name)

  val plusOp = token.collect:
    case Plus(tag) => Expr.Add.apply.curried(tag)

  val minusOp = token.collect:
    case Minus(tag) => Expr.Subtract.apply.curried(tag)

  val timesOp = token.collect:
    case Star(tag) => Expr.Multiply.apply.curried(tag)

  val divOp = token.collect:
    case Slash(tag) => Expr.Divide.apply.curried(tag)

  val andOp = token.collect:
    case And(tag) => Expr.And.apply.curried(tag)

  val orOp = token.collect:
    case Or(tag) => Expr.Or.apply.curried(tag)

  val equalEqualOp = token.collect:
    case EqualEqual(tag) => Expr.Equal.apply.curried(tag)

  val notEqualOp = token.collect:
    case BangEqual(tag) => Expr.NotEqual.apply.curried(tag)

  val lessOp = token.collect:
    case Less(tag) => Expr.Less.apply.curried(tag)

  val lessEqualOp = token.collect:
    case LessEqual(tag) => Expr.LessEqual.apply.curried(tag)

  val greaterOp = token.collect:
    case Greater(tag) => Expr.Greater.apply.curried(tag)

  val greaterEqualOp = token.collect:
    case GreaterEqual(tag) => Expr.GreaterEqual.apply.curried(tag)

  val groupStart = token.collect:
    case LeftParen(tag) => tag

  val groupEnd = token.collect:
    case RightParen(tag) => tag

  val unaryOp = token.collect:
    case Minus(tag) => Expr.Negate.apply.curried(tag)
    case Bang(tag)  => Expr.Not.apply.curried(tag)

  def binary(op: Parser[BinaryOp]): Parser[Expr] => Parser[Expr] =
    lazy val loop: (Expr => Expr) => Parser[BinaryOp] => Parser[Expr] => Parser[Expr] =
      // format: off
      u => op => expr =>
        for
          lhs <- expr.map(u)
          of <- op.?
          x <- of.fold(P.pure(lhs))(f => loop(f(lhs))(op)(expr))
        yield x

    loop(identity)(op)

  lazy val group = or.between(groupStart, groupEnd).map(Expr.Grouping.apply)
  lazy val primary = group | literal
  lazy val unary: Parser[Expr] = (unaryOp <*> unary).backtrack | primary
  lazy val factor = binary(timesOp | divOp)(unary)
  lazy val term = binary(plusOp | minusOp)(factor)
  lazy val comparison = binary(lessOp | lessEqualOp | greaterOp | greaterEqualOp)(term)
  lazy val equality = binary(equalEqualOp | notEqualOp)(comparison)
  lazy val and = binary(andOp)(equality)
  lazy val or = binary(orOp)(and)

  lazy val expr: Parser[Expr] = or <* P.end
