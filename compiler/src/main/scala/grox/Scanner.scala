package grox

import scala.util.control.NoStackTrace

import cats.*
import cats.data.NonEmptyList
import cats.implicits.*
import cats.parse.{Caret, LocationMap, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R}

trait Scanner[F[_]]:
  def scan(str: String): F[List[Token]]

object Scanner:

  def instance[F[_]: MonadThrow]: Scanner[F] = str => parse(str).map(_.map(_.token)).liftTo[F]

  val endOfLine: P[Unit] = R.cr | R.lf
  val whitespace: P[Unit] = endOfLine | R.wsp
  val whitespaces: P0[Unit] = P.until0(!whitespace).void

  // != | !
  val bangEqualOrBang: P[Operator] = Operator.BangEqual.parse | Operator.Bang.parse

  // == | =
  val equalEqualOrEqual: P[Operator] = Operator.EqualEqual.parse | Operator.Equal.parse

  // >= | >
  val greaterEqualOrGreater: P[Operator] = Operator.GreaterEqual.parse | Operator.Greater.parse

  // <= | <
  val lessEqualOrLess: P[Operator] = Operator.LessEqual.parse | Operator.Less.parse

  val keywords = Keyword.values.map(_.parse).toList
  // for testing purpose only
  val keyword: P[Keyword] = P.oneOf(keywords)

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

  val commentOrSlash: P[Token] = blockComment | singleLineComment | Operator.Slash.parse

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

  val allTokens =
    keywords ++ List(
      Operator.LeftParen.parse,
      Operator.RightParen.parse,
      Operator.LeftBrace.parse,
      Operator.RightBrace.parse,
      Operator.Comma.parse,
      Operator.Dot.parse,
      Operator.Minus.parse,
      Operator.Plus.parse,
      Operator.Semicolon.parse,
      Operator.Star.parse,
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

  val tokenInfo: P[TokenInfo] = (P.caret.with1 ~ token ~ P.caret).map { case ((s, t), e) =>
    TokenInfo(t, Span(s.toLocation, e.toLocation))
  }

  val parser = tokenInfo.rep.map(_.toList)

  def parse(str: String): Either[Error, List[TokenInfo]] =
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

  extension (o: Operator) def parse = P.string(o.lexeme).as(o)
  extension (k: Keyword) def parse = (P.string(k.lexeme) ~ (whitespace | P.end)).backtrack.as(k)
  extension (c: Caret) def toLocation: Location = Location(c.line, c.col, c.offset)

  extension [T](p: P[T])

    def span: P[(T, Span)] = (P.caret.with1 ~ p ~ P.caret).map { case ((s, t), e) =>
      (t, Span(s.toLocation, e.toLocation))
    }

  extension [T, U](p: P[T])

    def span2(f: (T, Span) => U): P[U] = (P.caret.with1 ~ p ~ P.caret).map { case ((s, t), e) =>
      f(t, Span(s.toLocation, e.toLocation))
    }
