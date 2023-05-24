package grox

import scala.util.control.NoStackTrace

import cats.syntax.all.*

import kantan.parsers.{Message, Parser as P, Position, SourceMap}

object TokenParser:

  case class Error(message: Message[Token[Span]]) extends NoStackTrace

  extension (l: Location) def position: Position = Position(l.line, l.col)

  given SourceMap[Token[Span]] = new:
    override def endsAt(token: Token[Span], current: Position) = token.tag.end.position
    override def startsAt(token: Token[Span], current: Position) = token.tag.start.position

  def run[A](p: Parser[A])(tokens: List[Token[Span]]) = p.parse(tokens).toEither.leftMap(Error(_))

  type Parser[A] = P[Token[Span], A]

  val token = P.token[Token[Span]]
