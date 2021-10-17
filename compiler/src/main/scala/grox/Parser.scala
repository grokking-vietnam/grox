package grox

import cats.parse.{Parser0 => P0, Parser => P, Rfc5234 => R, Numbers => N}
import cats.data.NonEmptyList

object Parser {

  val whitespace: P[Unit] = P.anyChar.filter(isSpace).void
  //val spaces: P[Unit] = P.charsWhile(isSpace _).void
  val whitespaces: P0[Unit] = P.until0(P.anyChar.filter(!isSpace(_))).void
  val maybeSpace: P0[Unit] = whitespaces.?.void

  val leftParen = operatorP(Operator.LeftParen)
  val rightParen = operatorP(Operator.RightParen)
  val leftBrace = operatorP(Operator.LeftBrace)
  val rightBrace = operatorP(Operator.RightBrace)
  val comma = operatorP(Operator.Comma)
  val dot = operatorP(Operator.Dot)
  val minus = operatorP(Operator.Minus)
  val plus = operatorP(Operator.Plus)
  val semicolon = operatorP(Operator.Semicolon)
  val slash = operatorP(Operator.Slash)
  val star = operatorP(Operator.Star)

  val bang = operatorP(Operator.Bang)
  val bangEqual = operatorP(Operator.BangEqual)
  val bangEqualOrBang = bangEqual | bang

  val equal = operatorP(Operator.Equal)
  val equalEqual = operatorP(Operator.EqualEqual)
  val equalEqualOrEqual = equalEqual | equal

  val greater = operatorP(Operator.Greater)
  val greaterEqual = operatorP(Operator.GreaterEqual)
  val greaterEqualOrGreater = greaterEqual | greater

  val less = operatorP(Operator.Less)
  val lessEqual = operatorP(Operator.LessEqual)
  val lessEqualOrLess = lessEqual | less

  // keywords
  val keywords = Keyword.values.map(keywordP).toList
  val keyword = P.oneOf(keywords)

  // todo support multiple lines comment
  val singleLineComment = P.string("//") *> P.until0(P.string("\n")).map(c => Comment.SingleLine(s"//$c"))
    val singleLineCommentOrSlash = singleLineComment | slash

  val alphaNumeric = R.alpha | N.digit | P.char('_').as('_')
  val identifier = ((R.alpha | P.char('_')) ~ alphaNumeric.rep0).map(p => p._1 :: p._2).string.map(Literal.Identifier(_))

  //val identifierOrKeyword: P[Token] = keyword | identifier

  val str = (R.dquote *> P.until0(R.dquote) <* R.dquote).map(Literal.Str(_))

  val frac = (P.char('.') *> N.digit.rep).map('.' :: _).backtrack
  val fracOrNone = frac.rep0(0, 1).map(_.flatMap(_.toList)).string
  val number = (N.digits ~ fracOrNone).map(p => p._1 + p._2).map(Literal.Number(_))
  //val numberOrDot = number | dot

  val allParsers = keywords ++ List(leftParen, rightParen, leftBrace, rightBrace, comma, dot, minus, plus, semicolon, slash, star, bangEqualOrBang, equalEqualOrEqual, greaterEqualOrGreater, lessEqualOrLess, singleLineCommentOrSlash, identifier, str, number)// ++ keywords
  val token: P[Token] = P.oneOf(allParsers.map(_ <* whitespaces))

  val parse = token.rep.map(_.toList).parseAll

  private def operatorP(token: Token) = P.string(token.lexeme).as(token)
  private def keywordP(keyword: Keyword) = keySpace(keyword.lexeme).as(keyword)

  // parse a keyword and some space or backtrack
  private def keySpace(str: String): P[Unit] =
    (P.string(str) ~ (whitespace | P.end)).void.backtrack
  //val spaces: P[Unit] = P.charsWhile(isSpace(_)).void

  private def isSpace(c: Char): Boolean =
    (c == ' ') || (c == '\t') || (c == '\r') || (c == '\n')

}
