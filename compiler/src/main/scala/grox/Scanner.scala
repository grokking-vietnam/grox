package grox

import scala.util.control.NoStackTrace

import cats.*
import cats.data.NonEmptyList
import cats.implicits.*
import cats.parse.{Caret, LocationMap, Numbers as N, Parser as P, Parser0 as P0, Rfc5234 as R}

trait Scanner[F[_]]:
  def scan(str: String): F[List[Token[Span]]]

object Scanner:

  import Keyword.*
  import Operator.*

  def instance[F[_]: MonadThrow]: Scanner[F] = str => parse(str).liftTo[F]

  val endOfLine: P[Unit] = R.cr | R.lf
  val whitespace: P[Unit] = endOfLine | R.wsp
  val whitespaces: P0[Unit] = P.until0(!whitespace).void

  // != | !
  val bangEqualOrBang: P[Operator[Unit]] = BangEqual(()).parse | Bang(()).parse

  // == | =
  val equalEqualOrEqual: P[Operator[Unit]] = EqualEqual(()).parse | Equal(()).parse

  // >= | >
  val greaterEqualOrGreater: P[Operator[Unit]] = GreaterEqual(()).parse | Greater(()).parse

  // <= | <
  val lessEqualOrLess: P[Operator[Unit]] = LessEqual(()).parse | Less(()).parse

  val keywords = List[Keyword[Unit]](
    And(()),
    Class(()),
    Else(()),
    False(()),
    For(()),
    Fun(()),
    If(()),
    Nil(()),
    Or(()),
    Print(()),
    Return(()),
    Super(()),
    This(()),
    True(()),
    Var(()),
    While(()),
    ).map(_.parse)

  val singleLineComment: P[Comment[Unit]] =
    val start = P.string("//")
    val line: P0[String] = P.until0(endOfLine)
    (start *> line).string.map(Comment.SingleLine(_, ()))

  val blockComment: P[Comment[Unit]] =
    val start = P.string("/*")
    val end = P.string("*/")
    val notStartOrEnd: P[Char] = (!(start | end)).with1 *> P.anyChar
    P.recursive[Comment.Block[Unit]] { recurse =>
      (start *>
        (notStartOrEnd | recurse).rep0
        <* end).string.map(Comment.Block(_, ()))
    }

  val commentOrSlash: P[Token[Unit]] =
    blockComment | singleLineComment | Operator.Slash(()).parse

  // An identifier can only start with an undercore or a letter
  // and can contain underscore or letter or numeric character
  val identifier: P[Literal[Unit]] =
    val alphaOrUnderscore = R.alpha | P.char('_')
    val alphaNumeric = alphaOrUnderscore | N.digit

    (alphaOrUnderscore ~ alphaNumeric.rep0)
      .string
      .map(Literal.Identifier(_, ()))

  val str: P[Literal[Unit]] = P
    .until0(R.dquote)
    .with1
    .surroundedBy(R.dquote)
    .map(Literal.Str(_, ()))

  // valid numbers: 1234 or 12.43
  // invalid numbers: .1234 or 1234.
  val number: P[Literal[Unit]] =
    val fraction = (P.char('.') *> N.digits).string.backtrack
    (N.digits ~ fraction.?)
      .string
      .map(Literal.Number(_, ()))

  val allTokens = keywords ++ List(
    Operator.LeftParen(()).parse,
    Operator.RightParen(()).parse,
    Operator.LeftBrace(()).parse,
    Operator.RightBrace(()).parse,
    Operator.Comma(()).parse,
    Operator.Dot(()).parse,
    Operator.Minus(()).parse,
    Operator.Plus(()).parse,
    Operator.Semicolon(()).parse,
    Operator.Star(()).parse,
    bangEqualOrBang,
    equalEqualOrEqual,
    greaterEqualOrGreater,
    lessEqualOrLess,
    commentOrSlash,
    identifier,
    str,
    number,
  )

  val token: P[Token[Unit]] = P.oneOf(allTokens).surroundedBy(whitespaces)
  val tokenWithTag: P[Token[Span]] = (P.caret.with1 ~ token ~ P.caret).map { case ((s, t), e) => t.switch(Span(s.toLocation, e.toLocation)) }
  val parser = tokenWithTag.rep.map(_.toList)

  def parse(str: String): Either[Error, List[Token[Span]]] =
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

  extension (o: Operator[Unit]) def parse = P.string(str).as(o)
  extension (k: Keyword[Unit]) def parse = (P.string(k.lexeme) ~ (whitespace | P.end)).backtrack.as(k)
  extension (c: Caret) def toLocation: Location = Location(c.line, c.col, c.offset)
