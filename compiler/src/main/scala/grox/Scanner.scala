package grox

import scala.util.control.NoStackTrace

import cats.*
import cats.data.NonEmptyList
import cats.implicits.*
import cats.parse.{Caret, LocationMap, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R}

trait Scanner[F[_]]:
  def scan(str: String): F[List[Token]]

object Scanner:

  def instance[F[_]: MonadThrow]: Scanner[F] = str => parse(str).liftTo[F]

  val endOfLine: P[Unit] = R.cr | R.lf
  val whitespace: P[Unit] = endOfLine | R.wsp
  val whitespaces: P0[Unit] = P.until0(!whitespace).void

  // != | !
  val bangEqualOrBang: P[Operator] =
    "!=".operator(Operator.BangEqual.apply) | "!".operator(Operator.Bang.apply)

  // == | =
  val equalEqualOrEqual: P[Operator] =
    "==".operator(Operator.EqualEqual.apply) | "=".operator(Operator.Equal.apply)

  // >= | >
  val greaterEqualOrGreater: P[Operator] =
    ">=".operator(Operator.GreaterEqual.apply) | ">".operator(Operator.Greater.apply)

  // <= | <
  val lessEqualOrLess: P[Operator] =
    "<=".operator(Operator.LessEqual.apply) | "<".operator(Operator.Less.apply)

  // val keywords = Keyword.values.map(_.parse).toList
  // for testing purpose only
  val keyword: P[Keyword] =
    "and".keyword(Keyword.And.apply) | "class".keyword(Keyword.Class.apply)
      | "else".keyword(Keyword.Else.apply) | "false".keyword(Keyword.False.apply)
      | "for".keyword(Keyword.For.apply) | "fun".keyword(Keyword.Fun.apply)
      | "if".keyword(Keyword.If.apply) | "nil".keyword(Keyword.Nil.apply)
      | "or".keyword(Keyword.Or.apply) | "print".keyword(Keyword.Print.apply)
      | "return".keyword(Keyword.Return.apply) | "super".keyword(Keyword.Super.apply)
      | "this".keyword(Keyword.This.apply) | "true".keyword(Keyword.True.apply)
      | "var".keyword(Keyword.Var.apply) | "while".keyword(Keyword.While.apply)

  val singleLineComment: P[Comment] =
    val start = P.string("//")
    val line: P0[String] = P.until0(endOfLine)
    (start *> line).string.span2(Comment.SingleLine.apply)

  val blockComment: P[Comment] =
    val start = P.string("/*")
    val end = P.string("*/")
    val notStartOrEnd: P[Char] = (!(start | end)).with1 *> P.anyChar
    P.recursive[Comment.Block] { recurse =>
      (start *>
        (notStartOrEnd | recurse).rep0
        <* end).string.span2(Comment.Block.apply)
    }

  val commentOrSlash: P[Token] =
    blockComment | singleLineComment | "/".operator(Operator.Slash.apply)

  // An identifier can only start with an undercore or a letter
  // and can contain underscore or letter or numeric character
  val identifier: P[Literal] =
    val alphaOrUnderscore = R.alpha | P.char('_')
    val alphaNumeric = alphaOrUnderscore | N.digit

    (alphaOrUnderscore ~ alphaNumeric.rep0)
      .string
      .span2(Literal.Identifier.apply)

  val str: P[Literal] = P
    .until0(R.dquote)
    .with1
    .surroundedBy(R.dquote)
    .span2(Literal.Str.apply)

  // valid numbers: 1234 or 12.43
  // invalid numbers: .1234 or 1234.
  val number: P[Literal] =
    val fraction = (P.char('.') *> N.digits).string.backtrack
    (N.digits ~ fraction.?)
      .string
      .span2(Literal.Number.apply)

  val allTokens = List(
    keyword,
    "(".operator(Operator.LeftParen.apply),
    ")".operator(Operator.RightParen.apply),
    "{".operator(Operator.LeftBrace.apply),
    "}".operator(Operator.RightBrace.apply),
    ",".operator(Operator.Comma.apply),
    ".".operator(Operator.Dot.apply),
    "-".operator(Operator.Minus.apply),
    "+".operator(Operator.Plus.apply),
    ":".operator(Operator.Semicolon.apply),
    "*".operator(Operator.Star.apply),
    bangEqualOrBang,
    equalEqualOrEqual,
    greaterEqualOrGreater,
    lessEqualOrLess,
    commentOrSlash,
    identifier,
    str,
    number,
  )

  val token: P[Token] = P.oneOf(allTokens).surroundedBy(whitespaces)

  val parser = token.rep.map(_.toList)

  def parse(str: String): Either[Error, List[Token]] =
    parser.parse(str) match
      case Right(("", ls)) => Right(ls)
      case Right((rest, ls)) =>
        val idx = str.indexOf(rest)
        val lm = LocationMap(str)
        Left(Error.PartialParse(ls, idx, lm))
      case Left(err) =>
        val idx = err.failedAtOffset
        val lm = LocationMap(str)
        Left(Error.ParseFailure(idx, lm))

  enum Error extends NoStackTrace:
    case PartialParse[A](got: A, position: Int, locations: LocationMap) extends Error
    case ParseFailure(position: Int, locations: LocationMap) extends Error

    override def toString: String =
      this match
        case PartialParse(_, pos, _) => s"PartialParse at $pos"
        case ParseFailure(pos, _)    => s"ParseFailure at $pos"

  extension (str: String) def operator(f: Span => Operator) = P.string(str).span(f)
  extension (str: String) def keyword(f: Span => Keyword) = P.string(str).span(f)
  extension (k: Keyword) def parse = (P.string(k.lexeme) ~ (whitespace | P.end)).backtrack.as(k)
  extension (c: Caret) def toLocation: Location = Location(c.line, c.col, c.offset)

  extension [T, U](p: P[T])

    def span(f: Span => U): P[U] = (P.caret.with1 ~ p ~ P.caret).map { case ((s, t), e) =>
      f(Span(s.toLocation, e.toLocation))
    }

  extension [T, U](p: P[T])

    def span2(f: (T, Span) => U): P[U] = (P.caret.with1 ~ p ~ P.caret).map { case ((s, t), e) =>
      f(t, Span(s.toLocation, e.toLocation))
    }
