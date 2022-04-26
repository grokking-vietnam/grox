package grox

case class Location(val line: Int, val col: Int, val offset: Int)
case class Span(start: Location, end: Location)

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

import Token.*

extension [A, B](t: Token[A])

  def switch(b: B): Token[B] =
    t match
      case Identifier(l, _) => Identifier(l, b)
      case Number(l, _)     => Number(l, b)
      case Str(l, _)        => Str(l, b)

      case LeftParen(_)  => LeftParen(b)
      case RightParen(_) => RightParen(b)
      case LeftBrace(_)  => LeftBrace(b)
      case RightBrace(_) => RightBrace(b)
      case Comma(_)      => Comma(b)
      case Dot(_)        => Dot(b)
      case Minus(_)      => Minus(b)
      case Plus(_)       => Plus(b)
      case Semicolon(_)  => Semicolon(b)
      case Slash(_)      => Slash(b)
      case Star(_)       => Star(b)

      case Bang(_)         => Bang(b)
      case BangEqual(_)    => BangEqual(b)
      case Equal(_)        => Equal(b)
      case EqualEqual(_)   => EqualEqual(b)
      case Greater(_)      => Greater(b)
      case GreaterEqual(_) => GreaterEqual(b)
      case Less(_)         => Less(b)
      case LessEqual(_)    => LessEqual(b)

      case And(_)    => And(b)
      case Class(_)  => Class(b)
      case Else(_)   => Else(b)
      case False(_)  => False(b)
      case For(_)    => For(b)
      case Fun(_)    => Fun(b)
      case If(_)     => If(b)
      case Null(_)   => Null(b)
      case Or(_)     => Or(b)
      case Print(_)  => Print(b)
      case Return(_) => Return(b)
      case Super(_)  => Super(b)
      case This(_)   => This(b)
      case True(_)   => True(b)
      case Var(_)    => Var(b)
      case While(_)  => While(b)

      case SingleLine(l, _) => SingleLine(l, b)
      case Block(l, _)      => Block(l, b)
