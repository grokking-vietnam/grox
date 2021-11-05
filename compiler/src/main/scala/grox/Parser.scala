package grox

import cats.data.NonEmptyList
import cats.parse.{LocationMap, Numbers => N, Parser => P, Parser0 => P0, Rfc5234 => R}

object Parser {

  val whitespace: P[Unit] = P.charIn(' ', '\t', '\n', '\r').void
  val whitespaces: P0[Unit] = P.until0(P.not(whitespace)).void

  val bangEqualOrBang = Operator.BangEqual.parse | Operator.Bang.parse

  val equalEqualOrEqual = Operator.EqualEqual.parse | Operator.Equal.parse

  val greaterEqualOrGreater = Operator.GreaterEqual.parse | Operator.Greater.parse

  val lessEqualOrLess = Operator.LessEqual.parse | Operator.Less.parse

  val keywords = Keyword.values.map(k => keySpace(k.lexeme).as(k)).toList
  val keyword = P.oneOf(keywords)

  val singleLineComment =
    P.string("//") *> P.until0(P.string("\n")).map(c => Comment.SingleLine(s"//$c"))

  val blockComment = (P.string("/*") *> P.until0(P.string("*/")) <* P.string("*/")).map(c =>
    Comment.Block(s"/*$c*/")
  )

  val commentOrSlash = blockComment | singleLineComment | Operator.Slash.parse

  val alphaNumeric = R.alpha | N.digit | P.char('_').as('_')

  val identifier = ((R.alpha | P.char('_')) ~ alphaNumeric.rep0)
    .map(p => p._1 :: p._2)
    .string
    .map(Literal.Identifier(_))

  val str = (R.dquote *> P.until0(R.dquote) <* R.dquote).map(Literal.Str(_))

  val frac = (P.char('.') *> N.digit.rep).map('.' :: _).backtrack
  val fracOrNone = frac.rep0(0, 1).map(_.flatMap(_.toList)).string
  val number = (N.digits ~ fracOrNone).map(p => p._1 + p._2).map(Literal.Number(_))

  val allParsers =
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

  val token: P[Token] = P.oneOf(allParsers.map(_ <* whitespaces))

  enum Error {
    case PartialParse[A](got: A, position: Int, locations: LocationMap) extends Error
    case ParseFailure(position: Int, locations: LocationMap) extends Error
  }

  val parser = whitespaces *> token.rep.map(_.toList)

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

  // parse a keyword and some space or backtrack
  private def keySpace(str: String): P[Unit] = (P.string(str) ~ (whitespace | P.end)).void.backtrack

  extension (o: Operator) def parse = P.string(o.lexeme).as(o)
}
