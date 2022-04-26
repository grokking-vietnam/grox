package grox

case class Location(val line: Int, val col: Int, val offset: Int)
case class Span(start: Location, end: Location)

enum Token[+T](val lexeme: String, val span: T):
  case Identifier(override val lexeme: String, override val span: T) extends Token(lexeme, span)
  case Str(override val lexeme: String, override val span: T) extends Token(lexeme, span)
  case Number(override val lexeme: String, override val span: T) extends Token(lexeme, span)

  // Single character token
  case LeftParen(override val span: T) extends Token("(", span)
  case RightParen(override val span: T) extends Token(")", span)
  case LeftBrace(override val span: T) extends Token("{", span)
  case RightBrace(override val span: T) extends Token("}", span)
  case Comma(override val span: T) extends Token(",", span)
  case Dot(override val span: T) extends Token(".", span)
  case Minus(override val span: T) extends Token("-", span)
  case Plus(override val span: T) extends Token("+", span)
  case Semicolon(override val span: T) extends Token(";", span)
  case Slash(override val span: T) extends Token("/", span)
  case Star(override val span: T) extends Token("*", span)

  // One or two character token
  case Bang(override val span: T) extends Token("!", span)
  case BangEqual(override val span: T) extends Token("!=", span)
  case Equal(override val span: T) extends Token("=", span)
  case EqualEqual(override val span: T) extends Token("==", span)
  case Greater(override val span: T) extends Token(">", span)
  case GreaterEqual(override val span: T) extends Token(">=", span)
  case Less(override val span: T) extends Token("<", span)
  case LessEqual(override val span: T) extends Token("<=", span)

  // keywords
  case And(override val span: T) extends Token("and", span)
  case Class(override val span: T) extends Token("class", span)
  case Else(override val span: T) extends Token("else", span)
  case False(override val span: T) extends Token("false", span)
  case For(override val span: T) extends Token("for", span)
  case Fun(override val span: T) extends Token("fun", span)
  case If(override val span: T) extends Token("if", span)
  case Nil(override val span: T) extends Token("nil", span)
  case Or(override val span: T) extends Token("or", span)
  case Print(override val span: T) extends Token("print", span)
  case Return(override val span: T) extends Token("return", span)
  case Super(override val span: T) extends Token("super", span)
  case This(override val span: T) extends Token("this", span)
  case True(override val span: T) extends Token("true", span)
  case Var(override val span: T) extends Token("var", span)
  case While(override val span: T) extends Token("while", span)

  case SingleLine(override val lexeme: String, override val span: T) extends Token(lexeme, span)
  case Block(override val lexeme: String, override val span: T) extends Token(lexeme, span)

import Token.*
extension[A, B](t: Token[A])

  def switch(b: B): Token[B] = t match
    case Identifier(l, _) => Identifier(l, b)
    case Number(l, _) => Number(l, b)
    case Str(l, _) => Str(l, b)

    case LeftParen(_) => LeftParen(b)
    case RightParen(_) => RightParen(b)
    case LeftBrace(_) => LeftBrace(b)
    case RightBrace(_) => RightBrace(b)
    case Comma(_) => Comma(b)
    case Dot(_) => Dot(b)
    case Minus(_) => Minus(b)
    case Plus(_) => Plus(b)
    case Semicolon(_) => Semicolon(b)
    case Slash(_) => Slash(b)
    case Star(_) => Star(b)

    case Bang(_) => Bang(b)
    case BangEqual(_) => BangEqual(b)
    case Equal(_) => Equal(b)
    case EqualEqual(_) => EqualEqual(b)
    case Greater(_) => Greater(b)
    case GreaterEqual(_) => GreaterEqual(b)
    case Less(_) => Less(b)
    case LessEqual(_) => LessEqual(b)

    case And(_) => And(b)
    case Class(_) => Class(b)
    case Else(_) => Else(b)
    case False(_) => False(b)
    case For(_) => For(b)
    case Fun(_) => Fun(b)
    case If(_) => If(b)
    case Nil(_) => Nil(b)
    case Or(_) => Or(b)
    case Print(_) => Print(b)
    case Return(_) => Return(b)
    case Super(_) => Super(b)
    case This(_) => This(b)
    case True(_) => True(b)
    case Var(_) => Var(b)
    case While(_) => While(b)

    case SingleLine(l, _) => SingleLine(l, b)
    case Block(l, _) => Block(l, b)

