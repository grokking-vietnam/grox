package grox

import cats.parse.{Parser0, Parser => P, Numbers}

object Parser {

  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val whitespaces: Parser0[Unit] = whitespace.rep.void

  val leftParen = tokenP(Token.LeftParen)
  val rightParen = tokenP(Token.RightParen)
  val leftBrace = tokenP(Token.LeftBrace)
  val rightBrace = tokenP(Token.RightBrace)
  val comma = tokenP(Token.Comma)
  val dot = tokenP(Token.Dot)
  val minus = tokenP(Token.Minus)
  val plus = tokenP(Token.Plus)
  val semicolon = tokenP(Token.Semicolon)
  val slash = tokenP(Token.Slash)

  val bang = tokenP(Token.Bang)
  val bangEqual = tokenP(Token.BangEqual)
  val bangEqualOrElseBang = bangEqual.orElse(bang)

  val equal = tokenP(Token.Equal)
  val equalEqual = tokenP(Token.EqualEqual)
  val equalEqualOrElseEqual = equalEqual.orElse(equal)

  val greater = tokenP(Token.Greater)
  val greaterEqual = tokenP(Token.GreaterEqual)
  val greaterEqualOrElseGreater = greaterEqual.orElse(greater)

  val less = tokenP(Token.Less)
  val lessEqual = tokenP(Token.LessEqual)
  val lessEqualOrElseLess = lessEqual.orElse(less)

  private def tokenP(token: Token) = P.string(token.lexeme).as(token)
}
