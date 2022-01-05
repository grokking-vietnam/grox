package grox

sealed trait MyOperator
sealed trait MyUnary extends MyOperator
sealed trait MyLiteral
sealed trait Mykeywords

enum MyToken[+T](val lexeme: String):
  // Literal
  case Identifier(override val lexeme: String) extends MyToken[MyLiteral](lexeme)
  case Str(override val lexeme: String) extends MyToken[MyLiteral](lexeme)
  case Number(override val lexeme: String) extends MyToken[MyLiteral](lexeme)

  case LeftParen extends MyToken[MyOperator]("(")
  case RightParen extends MyToken[MyOperator](")")
  case Dot extends MyToken[MyOperator](".")
  case Minus extends MyToken[MyUnary]("-")
  case Plus extends MyToken[MyOperator]("+")
  case Semicolon extends MyToken[MyOperator](";")
  case Slash extends MyToken[MyOperator]("/")
  case Star extends MyToken[MyOperator]("*")

  // One or two character token
  case Bang extends MyToken[MyUnary]("!")
  case BangEqual extends MyToken[MyOperator]("!=")
  case Equal extends MyToken[MyOperator]("=")
  case EqualEqual extends MyToken[MyOperator]("==")
  case Greater extends MyToken[MyOperator](">")
  case GreaterEqual extends MyToken[MyOperator](">=")
  case Less extends MyToken[MyOperator]("<")
  case LessEqual extends MyToken[MyOperator]("<=")
