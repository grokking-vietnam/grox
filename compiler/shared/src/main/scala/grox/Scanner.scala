package grox

import scala.util.control.NoStackTrace

import cats.*
import cats.data.NonEmptyList
import cats.parse.{Caret, LocationMap, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R}
import cats.syntax.all.*

import Token.*

trait Scanner[F[_]]:
  def scan(str: String): F[List[Token[Span]]]

object Scanner:

  def instance[F[_]: MonadThrow]: Scanner[F] = str => parse(str).liftTo[F]

  val endOfLine: P[Unit] = R.cr | R.lf
  val whitespace: P[Unit] = endOfLine | R.wsp
  val whitespaces: P0[Unit] = P.until0(!whitespace).void
  val location = P.caret.map(c => Location(c.line, c.col, c.offset))
  val alphaOrUnderscore = R.alpha | P.char('_')
  val alphaNumeric = alphaOrUnderscore | N.digit

  // != | !
  val bangEqualOrBang: P[Token[Unit]] = BangEqual(()).operator | Bang(()).operator

  // == | =
  val equalEqualOrEqual: P[Token[Unit]] = EqualEqual(()).operator | Equal(()).operator

  // >= | >
  val greaterEqualOrGreater: P[Token[Unit]] = GreaterEqual(()).operator | Greater(()).operator

  // <= | <
  val lessEqualOrLess: P[Token[Unit]] = LessEqual(()).operator | Less(()).operator

  val keywordsParser = keywords.map(_.keyword)
  val keyword = P.oneOf(keywordsParser)

  val singleLineComment: P[Token[Unit]] =
    val start = P.string("//")
    val line: P0[String] = P.until0(endOfLine)
    (start ~ line).string.map(SingleLine(_, ()))

  val blockComment: P[Token[Unit]] =
    val start = P.string("/*")
    val end = P.string("*/")
    val notStartOrEnd: P[Char] = (!(start | end)).with1 *> P.anyChar
    P.recursive[Block[Unit]] { recurse =>
      (start *>
        (notStartOrEnd | recurse).rep0
        <* end).string.map(Block(_, ()))
    }

  val commentOrSlash: P[Token[Unit]] = blockComment | singleLineComment | Slash(()).operator

  // An identifier can only start with an undercore or a letter
  // and can contain underscore or letter or numeric character
  val identifier: P[Token[Unit]] = (alphaOrUnderscore ~ alphaNumeric.rep0)
    .string
    .map(Identifier(_, ()))

  val str: P[Token[Unit]] = P
    .until0(R.dquote)
    .with1
    .surroundedBy(R.dquote)
    .map(Str(_, ()))

  // valid numbers: 1234 or 12.43
  // invalid numbers: .1234 or 1234.
  val number: P[Token[Unit]] =
    val fraction = (P.char('.') *> N.digits).string.backtrack
    (N.digits ~ fraction.?)
      .string
      .map(Number(_, ()))

  val allTokens: List[P[Token[Unit]]] =
    keywordsParser ++ List(
      LeftParen(()).operator,
      RightParen(()).operator,
      LeftBrace(()).operator,
      RightBrace(()).operator,
      Comma(()).operator,
      Dot(()).operator,
      Minus(()).operator,
      Plus(()).operator,
      Semicolon(()).operator,
      Star(()).operator,
      bangEqualOrBang,
      equalEqualOrEqual,
      greaterEqualOrGreater,
      lessEqualOrLess,
      commentOrSlash,
      identifier,
      str,
      number,
    )

  val token: P[Token[Span]] = P.oneOf(allTokens).span.surroundedBy(whitespaces)

  val parser = token.rep.map(_.toList)

  def parse(str: String): Either[Error, List[Token[Span]]] =
    parser.parse(str) match
      case Right("", ls) => Right(ls)
      case Right(rest, ls) =>
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

  extension (t: Token[Unit])
    def operator = P.string(t.lexeme).as(t)

    // The character that immediately follows a keyword must not be a alphanumberic character.
    //
    // A keyword should be followed by a non-alphanumberic character.
    def keyword = (P.string(t.lexeme).as(t) <* (!alphaNumeric).peek).backtrack

  extension (p: P[Token[Unit]])
    def span = (location.with1 ~ p ~ location).map { case ((s, t), e) => t.as(Span(s, e)) }
