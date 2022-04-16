package grox

case class Location(val line: Int, val col: Int, val offset: Int)
case class Span(start: Location, end: Location)

// todo use opaque type for lexeme
sealed trait Token:
  val lexeme: String
  //val span: Span

case class TokenInfo(start: Location, token: Token, end: Location)
case class TokenInfo1(token: Token, span: Span)

enum Literal extends Token:

  case Identifier(val lexeme: String)
  case Str(val lexeme: String)
  case Number(val lexeme: String)

enum Operator(val lexeme: String, loc: Location = Location(0, 0, 0)) extends Token:

  // Single character token
  case LeftParen extends Operator("(")
  case RightParen extends Operator(")")
  case LeftBrace extends Operator("{")
  case RightBrace extends Operator("}")
  case Comma extends Operator(",")
  case Dot extends Operator(".")
  case Minus extends Operator("-")
  case Plus extends Operator("+")
  case Semicolon extends Operator(";")
  case Slash extends Operator("/")
  case Star extends Operator("*")

  // One or two character token
  case Bang extends Operator("!")
  case BangEqual extends Operator("!=")
  case Equal extends Operator("=")
  case EqualEqual extends Operator("==")
  case Greater extends Operator(">")
  case GreaterEqual extends Operator(">=")
  case Less extends Operator("<")
  case LessEqual extends Operator("<=")

enum Keyword(val lexeme: String) extends Token:
  case And extends Keyword("and")
  case Class extends Keyword("class")
  case Else extends Keyword("else")
  case False extends Keyword("false")
  case For extends Keyword("for")
  case Fun extends Keyword("fun")
  case If extends Keyword("if")
  case Nil extends Keyword("nil")
  case Or extends Keyword("or")
  case Print extends Keyword("print")
  case Return extends Keyword("return")
  case Super extends Keyword("super")
  case This extends Keyword("this")
  case True extends Keyword("true")
  case Var extends Keyword("var")
  case While extends Keyword("while")

enum Comment extends Token:
  case SingleLine(val lexeme: String)
  case Block(val lexeme: String)
