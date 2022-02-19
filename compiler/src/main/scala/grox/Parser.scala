package grox


object Parser {

  def parse[T](tokens: List[Token]): Either[Error, (List[Token], Expr[T])] = ???

  case class Error(failedAt: Int, reason: String)

}
