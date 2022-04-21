package grox

case class Location(val line: Int, val col: Int, val offset: Int)
case class Span(start: Location, end: Location)

// todo use opaque type for lexeme
sealed trait Token[+T]:
  val lexeme: String
  val span: T

enum Literal[+T] extends Token[T]:

  case Identifier(val lexeme: String, val span: T)
  case Str(val lexeme: String, val span: T)
  case Number(val lexeme: String, val span: T)

enum Operator[+T](val lexeme: String) extends Token[T]:

  // Single character token
  case LeftParen(val span: T) extends Operator[T]("(")
  case RightParen(val span: T) extends Operator[T](")")
  case LeftBrace(val span: T) extends Operator[T]("{")
  case RightBrace(val span: T) extends Operator[T]("}")
  case Comma(val span: T) extends Operator[T](",")
  case Dot(val span: T) extends Operator[T](".")
  case Minus(val span: T) extends Operator[T]("-")
  case Plus(val span: T) extends Operator[T]("+")
  case Semicolon(val span: T) extends Operator[T](";")
  case Slash(val span: T) extends Operator[T]("/")
  case Star(val span: T) extends Operator[T]("*")

  // One or two character token
  case Bang(val span: T) extends Operator[T]("!")
  case BangEqual(val span: T) extends Operator[T]("!=")
  case Equal(val span: T) extends Operator[T]("=")
  case EqualEqual(val span: T) extends Operator[T]("==")
  case Greater(val span: T) extends Operator[T](">")
  case GreaterEqual(val span: T) extends Operator[T](">=")
  case Less(val span: T) extends Operator[T]("<")
  case LessEqual(val span: T) extends Operator[T]("<=")

enum Keyword[+T](val lexeme: String) extends Token[T]:
  case And(val span: T) extends Keyword[T]("and")
  case Class(val span: T) extends Keyword[T]("class")
  case Else(val span: T) extends Keyword[T]("else")
  case False(val span: T) extends Keyword[T]("false")
  case For(val span: T) extends Keyword[T]("for")
  case Fun(val span: T) extends Keyword[T]("fun")
  case If(val span: T) extends Keyword[T]("if")
  case Nil(val span: T) extends Keyword[T]("nil")
  case Or(val span: T) extends Keyword[T]("or")
  case Print(val span: T) extends Keyword[T]("print")
  case Return(val span: T) extends Keyword[T]("return")
  case Super(val span: T) extends Keyword[T]("super")
  case This(val span: T) extends Keyword[T]("this")
  case True(val span: T) extends Keyword[T]("true")
  case Var(val span: T) extends Keyword[T]("var")
  case While(val span: T) extends Keyword[T]("while")

enum Comment[T] extends Token[T]:
  case SingleLine(val lexeme: String, val span: T)
  case Block(val lexeme: String, val span: T)

enum Keyword2(val lexeme: String):
  case And(val span: Span) extends Keyword2("and")

import Keyword.*
import Operator.*
import Comment.*
import Literal.*

extension[T, U](t: Token[T])

  def switch(u: U): Token[U] = t match
    case Identifier(l, _) => Identifier(l, u)
    case Number(l, _) => Number(l, u)
    case Str(l, _) => Str(l, u)
    case Or(_) => Or(u)

