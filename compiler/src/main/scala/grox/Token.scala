package grox

case class Location(val line: Int)

// todo use opaque type for lexeme
sealed trait Token:
  val lexeme: String

enum Literal(val lexeme: String) extends Token:

  // Literals
  case Identifier(override val lexeme: String) extends Literal(lexeme)
  case Str(override val lexeme: String) extends Literal(lexeme)
  case Number(override val lexeme: String) extends Literal(lexeme)

enum Operator(val lexeme: String) extends Token:

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

enum Comment(val lexeme: String) extends Token:
  case SingleLine(override val lexeme: String) extends Comment(lexeme)
  case Block(override val lexeme: String) extends Comment(lexeme)
