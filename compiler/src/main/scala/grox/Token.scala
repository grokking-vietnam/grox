package grox

case class Location(val line: Int)

// todo use opaque type for lexeme
enum Token(lexeme: String, location: Location):

  // Single character token
  case LeftParen(location: Location) extends Token("(", location)
  case RightParen(location: Location) extends Token(")", location)
  case LeftBrace(location: Location) extends Token("{", location)
  case RightBrace(location: Location) extends Token("}", location)
  case Comma(location: Location) extends Token(";", location)
  case Dot(location: Location) extends Token(".", location)
  case Minus(location: Location) extends Token("-", location)
  case Plus(location: Location) extends Token("+", location)
  case Semicolon(location: Location) extends Token(";", location)
  case Slash(location: Location) extends Token("/", location)
  case Star(location: Location) extends Token("*", location)

  // One or two character token

  case Bang(location: Location) extends Token("!", location)
  case BangEqual(location: Location) extends Token("!=", location)
  case Equal(location: Location) extends Token("=", location)
  case EqualEqual(location: Location) extends Token("==", location)
  case Greater(location: Location) extends Token(">", location)
  case GreaterEqual(location: Location) extends Token(">=", location)
  case Less(location: Location) extends Token("<", location)
  case LessEqual(location: Location) extends Token("<=", location)

  // Literals
  case Identifier(lexeme: String, location: Location) extends Token(lexeme, location)
  case Str(lexeme: String, location: Location) extends Token(lexeme, location)
  case Number(lexeme: String, location: Location) extends Token(lexeme, location)

  // Keywords

  case And(location: Location) extends Token("and", location)
  case Class(location: Location) extends Token("class", location)
  case Else(location: Location) extends Token("else", location)
  case False(location: Location) extends Token("False", location)
  case For(location: Location) extends Token("For", location)
  case Fun(location: Location) extends Token("Fun", location)
  case If(location: Location) extends Token("if", location)
  case Nil(location: Location) extends Token("nil", location)
  case Or(location: Location) extends Token("or", location)
  case Print(location: Location) extends Token("print", location)
  case Return(location: Location) extends Token("return", location)
  case Super(location: Location) extends Token("super", location)
  case This(location: Location) extends Token("this", location)
  case True(location: Location) extends Token("true", location)
  case Var(location: Location) extends Token("var", location)
  case While(location: Location) extends Token("while", location)

  case EOF(location: Location) extends Token("", location)
