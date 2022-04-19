package grox

case class Location(val line: Int, val col: Int, val offset: Int)
case class Span(start: Location, end: Location)

// todo use opaque type for lexeme
sealed trait Token:
  val lexeme: String
  val span: Span

case class TokenInfo(token: Token, span: Span)

enum Literal extends Token:

  case Identifier(val lexeme: String, val span: Span)
  case Str(val lexeme: String, val span: Span)
  case Number(val lexeme: String, val span: Span)

enum Operator(val lexeme: String) extends Token:

  // Single character token
  case LeftParen(val span: Span) extends Operator("(")
  case RightParen(val span: Span) extends Operator(")")
  case LeftBrace(val span: Span) extends Operator("{")
  case RightBrace(val span: Span) extends Operator("}")
  case Comma(val span: Span) extends Operator(",")
  case Dot(val span: Span) extends Operator(".")
  case Minus(val span: Span) extends Operator("-")
  case Plus(val span: Span) extends Operator("+")
  case Semicolon(val span: Span) extends Operator(";")
  case Slash(val span: Span) extends Operator("/")
  case Star(val span: Span) extends Operator("*")

  // One or two character token
  case Bang(val span: Span) extends Operator("!")
  case BangEqual(val span: Span) extends Operator("!=")
  case Equal(val span: Span) extends Operator("=")
  case EqualEqual(val span: Span) extends Operator("==")
  case Greater(val span: Span) extends Operator(">")
  case GreaterEqual(val span: Span) extends Operator(">=")
  case Less(val span: Span) extends Operator("<")
  case LessEqual(val span: Span) extends Operator("<=")

enum Keyword(val lexeme: String) extends Token:
  case And(val span: Span) extends Keyword("and")
  case Class(val span: Span) extends Keyword("class")
  case Else(val span: Span) extends Keyword("else")
  case False(val span: Span) extends Keyword("false")
  case For(val span: Span) extends Keyword("for")
  case Fun(val span: Span) extends Keyword("fun")
  case If(val span: Span) extends Keyword("if")
  case Nil(val span: Span) extends Keyword("nil")
  case Or(val span: Span) extends Keyword("or")
  case Print(val span: Span) extends Keyword("print")
  case Return(val span: Span) extends Keyword("return")
  case Super(val span: Span) extends Keyword("super")
  case This(val span: Span) extends Keyword("this")
  case True(val span: Span) extends Keyword("true")
  case Var(val span: Span) extends Keyword("var")
  case While(val span: Span) extends Keyword("while")

enum Comment extends Token:
  case SingleLine(val lexeme: String, val span: Span)
  case Block(val lexeme: String, val span: Span)

enum Keyword2(val lexeme: String):
  case And(val span: Span) extends Keyword2("and")
