package grox

case class Location(val line: Int)

// todo use opaque type for lexeme
enum Token(val lexeme: String):

  // Single character token
  case LeftParen extends Token("(")
  case RightParen extends Token(")")
  case LeftBrace extends Token("{")
  case RightBrace extends Token("}")
  case Comma extends Token(",")
  case Dot extends Token(".")
  case Minus extends Token("-")
  case Plus extends Token("+")
  case Semicolon extends Token(";")
  case Slash extends Token("/")
  case Star extends Token("*")

  // One or two character token

  case Bang extends Token("!")
  case BangEqual extends Token("!=")
  case Equal extends Token("=")
  case EqualEqual extends Token("==")
  case Greater extends Token(">")
  case GreaterEqual extends Token(">=")
  case Less extends Token("<")
  case LessEqual extends Token("<=")

  // Literals
  case Identifier(override val lexeme: String) extends Token(lexeme)
  case Str(override val lexeme: String) extends Token(lexeme)
  case Number(override val lexeme: String) extends Token(lexeme)

  // Keywords
  case And extends Token("and")
  case Class extends Token("class")
  case Else extends Token("else")
  case False extends Token("false")
  case For extends Token("for")
  case Fun extends Token("fun")
  case If extends Token("if")
  case Nil extends Token("nil")
  case Or extends Token("or")
  case Print extends Token("print")
  case Return extends Token("return")
  case Super extends Token("super")
  case This extends Token("this")
  case True extends Token("true")
  case Var extends Token("var")
  case While extends Token("while")

  case EOF extends Token("")
