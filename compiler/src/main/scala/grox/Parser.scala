package grox

import cats.data.NonEmptyList
import cats.parse.{Numbers => N, Parser => P, Parser0 => P0, Rfc5234 => R}

object Parser {

  val whitespace: P[Unit] = P.anyChar.filter(isSpace).void
  val whitespaces: P0[Unit] = P.until0(P.anyChar.filter(!isSpace(_))).void
  val maybeSpace: P0[Unit] = whitespaces.?.void

  val bangEqualOrBang = Operator.BangEqual.parse | Operator.Bang.parse

  val equalEqualOrEqual = Operator.EqualEqual.parse | Operator.Equal.parse

  val greaterEqualOrGreater = Operator.GreaterEqual.parse | Operator.Greater.parse

  val lessEqualOrLess = Operator.LessEqual.parse | Operator.Less.parse

  // keywords
  val keywords = Keyword.values.map(k => keySpace(k.lexeme).as(k)).toList
  val keyword = P.oneOf(keywords)

  // todo support multiple lines comment
  val singleLineComment =
    P.string("//") *> P.until0(P.string("\n")).map(c => Comment.SingleLine(s"//$c"))
  val singleLineCommentOrSlash = singleLineComment | Operator.Slash.parse

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
      singleLineCommentOrSlash,
      identifier,
      str,
      number,
    )

  val token: P[Token] = P.oneOf(allParsers.map(_ <* whitespaces))

  val parse = (maybeSpace *> token.rep.map(_.toList)).parseAll

  // parse a keyword and some space or backtrack
  private def keySpace(str: String): P[Unit] = (P.string(str) ~ (whitespace | P.end)).void.backtrack
  private def isSpace(c: Char): Boolean = (c == ' ') || (c == '\t') || (c == '\r') || (c == '\n')

  extension (o: Operator) def parse = P.string(o.lexeme).as(o)
}
