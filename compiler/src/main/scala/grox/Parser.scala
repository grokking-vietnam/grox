package grox

object Parser {

  // final def parse(str: String): Either[Parser.Error, (String, A)] = {
  def parse[T](tokens: List[Token]): Either[Error, (List[Token], Expr[T])] = ???

  case class Error(failedAt: Int, reason: String)

}
