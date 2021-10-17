package grox

import cats.parse.{Parser0, Parser => P, Rfc5234}

object Parser {

  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val whitespaces: Parser0[Unit] = whitespace.rep.void

  val leftParen = tokenP(Operator.LeftParen)
  val rightParen = tokenP(Operator.RightParen)
  val leftBrace = tokenP(Operator.LeftBrace)
  val rightBrace = tokenP(Operator.RightBrace)
  val comma = tokenP(Operator.Comma)
  val dot = tokenP(Operator.Dot)
  val minus = tokenP(Operator.Minus)
  val plus = tokenP(Operator.Plus)
  val semicolon = tokenP(Operator.Semicolon)
  val slash = tokenP(Operator.Slash)

  val bang = tokenP(Operator.Bang)
  val bangEqual = tokenP(Operator.BangEqual)
  val bangEqualOrElseBang = bangEqual | bang

  val equal = tokenP(Operator.Equal)
  val equalEqual = tokenP(Operator.EqualEqual)
  val equalEqualOrElseEqual = equalEqual | equal

  val greater = tokenP(Operator.Greater)
  val greaterEqual = tokenP(Operator.GreaterEqual)
  val greaterEqualOrElseGreater = greaterEqual | greater

  val less = tokenP(Operator.Less)
  val lessEqual = tokenP(Operator.LessEqual)
  val lessEqualOrElseLess = lessEqual | less

  // todo support multiple lines comment
  val singleLineComment = P.string("//") *> P.anyChar.repUntil0(P.string("\n")).string.map(c => Comment.SingleLine(s"//$c"))
    val singleLineCommentOrElseSlash = singleLineComment | slash

  // keywords
  val keywords = Keyword.values.map(tokenP).toList
  val keyword = P.oneOf(keywords)

  val alphaNumeric = Rfc5234.alpha | Rfc5234.digit | P.char('_').as('_')
  val identifier = (Rfc5234.alpha ~ alphaNumeric.rep0).map(p => p._1 :: p._2).string.map(Literal.Identifier(_))



  private def tokenP(token: Token) = P.string(token.lexeme).as(token)
}
