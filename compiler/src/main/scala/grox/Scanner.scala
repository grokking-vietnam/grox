package grox

import cats._
import cats.data.NonEmptyList
import cats.implicits._
import cats.mtl._
import cats.mtl.implicits._
import cats.parse.{LocationMap, Numbers => N, Parser => P, Parser0 => P0, Rfc5234 => R}

trait Scanner[F[_]] {
  def parse(str: String): F[List[Token]]
}

object Scanner {

  def instance[F[_]](implicit FR: Raise[F, Scanner.Error], A: Applicative[F]): Scanner[F] =
    new Scanner {
      def parse(str: String) = Scanner.parse(str).fold(FR.raise, A.pure)

    }

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

  val keywords: List[P[Keyword]] = Keyword.values.map(_.parse).toList
  // for testing purpose only
  val keyword: P[Keyword] = P.oneOf(keywords)

  val singleLineComment: P[Comment] = {
    val start = P.string("//")
    val line: P0[String] = P.until0(endOfLine)
    (start *> line).string.map(Comment.SingleLine(_))
  }

  val blockComment: P[Comment] =
    val start = P.string("/*")
    val end = P.string("*/")
    val notStartOrEnd: P[Char] = (!(start | end)).with1 *> P.anyChar
    P.recursive[Comment.Block] { recurse =>
      (start *>
        (notStartOrEnd | recurse).rep0
        <* end).string.map(Comment.Block(_))
    }

  val commentOrSlash: P[Token] = blockComment | singleLineComment | Operator.Slash.parse

  // An identifier can only start with an undercore or a letter
  // and can contain underscore or letter or numeric character
  val identifier: P[Literal] = {
    val alphaOrUnderscore = R.alpha | P.char('_')
    val alphaNumeric = alphaOrUnderscore | N.digit

    (alphaOrUnderscore ~ alphaNumeric.rep0)
      .string
      .map(Literal.Identifier(_))
  }

  val str: P[Literal] = P.until0(R.dquote).with1.surroundedBy(R.dquote).map(Literal.Str(_))

  // valid numbers: 1234 or 12.43
  // invalid numbers: .1234 or 1234.
  val number: P[Literal] = {
    val fraction = (P.char('.') *> N.digits).string.backtrack
    (N.digits ~ fraction.?).string.map(Literal.Number(_))
  }

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

  val parser = token.rep.map(_.toList)

  def parse(str: String): Either[Error, List[Token]] = {
    val lm = LocationMap(str)
    parser.parse(str) match {
      case Right(("", ls)) => Right(ls)
      case Right((rest, ls)) =>
        val idx = str.indexOf(rest)
        Left(Error.PartialParse(ls, idx, lm))
      case Left(err) =>
        val idx = err.failedAtOffset
        Left(Error.ParseFailure(idx, lm))
    }
  }

  enum Error {
    case PartialParse[A](got: A, position: Int, locations: LocationMap) extends Error
    case ParseFailure(position: Int, locations: LocationMap) extends Error
  }

  extension (o: Operator) def parse = P.string(o.lexeme).as(o)
  extension (k: Keyword) def parse = (P.string(k.lexeme) ~ (whitespace | P.end)).backtrack.as(k)
}
