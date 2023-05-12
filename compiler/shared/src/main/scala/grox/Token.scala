package grox

import cats.Functor

case class Location(val line: Int, val col: Int, val offset: Int)
case class Span(start: Location, end: Location)

object Span:
  val empty = Span(Location(0, 0, 0), Location(0, 0, 0))

object Location:
  val empty = Location(0, 0, 0)

enum Token[+T](val lexeme: String, val tag: T):
  case Identifier(override val lexeme: String, override val tag: T) extends Token(lexeme, tag)
  case Str(override val lexeme: String, override val tag: T) extends Token(lexeme, tag)
  case Number(override val lexeme: String, override val tag: T) extends Token(lexeme, tag)

  // Single character token
  case LeftParen(override val tag: T) extends Token("(", tag)
  case RightParen(override val tag: T) extends Token(")", tag)
  case LeftBrace(override val tag: T) extends Token("{", tag)
  case RightBrace(override val tag: T) extends Token("}", tag)
  case Comma(override val tag: T) extends Token(",", tag)
  case Dot(override val tag: T) extends Token(".", tag)
  case Minus(override val tag: T) extends Token("-", tag)
  case Plus(override val tag: T) extends Token("+", tag)
  case Semicolon(override val tag: T) extends Token(";", tag)
  case Slash(override val tag: T) extends Token("/", tag)
  case Star(override val tag: T) extends Token("*", tag)

  // One or two character token
  case Bang(override val tag: T) extends Token("!", tag)
  case BangEqual(override val tag: T) extends Token("!=", tag)
  case Equal(override val tag: T) extends Token("=", tag)
  case EqualEqual(override val tag: T) extends Token("==", tag)
  case Greater(override val tag: T) extends Token(">", tag)
  case GreaterEqual(override val tag: T) extends Token(">=", tag)
  case Less(override val tag: T) extends Token("<", tag)
  case LessEqual(override val tag: T) extends Token("<=", tag)

  // keywords
  case And(override val tag: T) extends Token("and", tag)
  case Class(override val tag: T) extends Token("class", tag)
  case Else(override val tag: T) extends Token("else", tag)
  case False(override val tag: T) extends Token("false", tag)
  case For(override val tag: T) extends Token("for", tag)
  case Fun(override val tag: T) extends Token("fun", tag)
  case If(override val tag: T) extends Token("if", tag)
  case Null(override val tag: T) extends Token("nil", tag)
  case Or(override val tag: T) extends Token("or", tag)
  case Print(override val tag: T) extends Token("print", tag)
  case Return(override val tag: T) extends Token("return", tag)
  case Super(override val tag: T) extends Token("super", tag)
  case This(override val tag: T) extends Token("this", tag)
  case True(override val tag: T) extends Token("true", tag)
  case Var(override val tag: T) extends Token("var", tag)
  case While(override val tag: T) extends Token("while", tag)

  case SingleLine(override val lexeme: String, override val tag: T) extends Token(lexeme, tag)
  case Block(override val lexeme: String, override val tag: T) extends Token(lexeme, tag)

object Token:

  import Token.*

  val keywords = List(
    And(()),
    Class(()),
    Else(()),
    False(()),
    For(()),
    Fun(()),
    If(()),
    Null(()),
    Or(()),
    Print(()),
    Return(()),
    Super(()),
    This(()),
    True(()),
    Var(()),
    While(()),
  )

  val operators = List(
    LeftParen(()),
    RightParen(()),
    LeftBrace(()),
    RightBrace(()),
    Comma(()),
    Dot(()),
    Minus(()),
    Plus(()),
    Semicolon(()),
    Slash(()),
    Star(()),
    Bang(()),
    BangEqual(()),
    Equal(()),
    EqualEqual(()),
    Greater(()),
    GreaterEqual(()),
    Less(()),
    LessEqual(()),
  )

  given Functor[Token] with

    def map[A, B](token: Token[A])(f: A => B): Token[B] = token match
      case Identifier(l, a) => Identifier(l, f(a))
      case Number(l, a)     => Number(l, f(a))
      case Str(l, a)        => Str(l, f(a))

      case LeftParen(a)  => LeftParen(f(a))
      case RightParen(a) => RightParen(f(a))
      case LeftBrace(a)  => LeftBrace(f(a))
      case RightBrace(a) => RightBrace(f(a))
      case Comma(a)      => Comma(f(a))
      case Dot(a)        => Dot(f(a))
      case Minus(a)      => Minus(f(a))
      case Plus(a)       => Plus(f(a))
      case Semicolon(a)  => Semicolon(f(a))
      case Slash(a)      => Slash(f(a))
      case Star(a)       => Star(f(a))

      case Bang(a)         => Bang(f(a))
      case BangEqual(a)    => BangEqual(f(a))
      case Equal(a)        => Equal(f(a))
      case EqualEqual(a)   => EqualEqual(f(a))
      case Greater(a)      => Greater(f(a))
      case GreaterEqual(a) => GreaterEqual(f(a))
      case Less(a)         => Less(f(a))
      case LessEqual(a)    => LessEqual(f(a))

      case And(a)    => And(f(a))
      case Class(a)  => Class(f(a))
      case Else(a)   => Else(f(a))
      case False(a)  => False(f(a))
      case For(a)    => For(f(a))
      case Fun(a)    => Fun(f(a))
      case If(a)     => If(f(a))
      case Null(a)   => Null(f(a))
      case Or(a)     => Or(f(a))
      case Print(a)  => Print(f(a))
      case Return(a) => Return(f(a))
      case Super(a)  => Super(f(a))
      case This(a)   => This(f(a))
      case True(a)   => True(f(a))
      case Var(a)    => Var(f(a))
      case While(a)  => While(f(a))

      case SingleLine(l, a) => SingleLine(l, f(a))
      case Block(l, a)      => Block(l, f(a))
