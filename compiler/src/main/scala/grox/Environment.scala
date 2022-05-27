package grox

import cats.implicits.catsSyntaxEither

object Environment {
  def apply(): Environment = new Environment(Map.empty[String, Token[Unit]], enclosing = None)
}

enum EnvironmentError(msg: String):
  case UndefinedVariableError(variable: Token[Unit])
    extends EnvironmentError(s"Undefined variable: '$variable'.")

class Environment(
  private val values: Map[String, Token[Unit]],
  private val enclosing: Option[Environment],
) {

  def define(name: String, value: Token[Unit]): Environment =
    new Environment(values + (name -> value), enclosing)

  def get(
    name: Token[Unit]
  ): Option[Token[Unit]] = values.get(name.lexeme).orElse(enclosing.flatMap(_.get(name)))

  def assign(name: Token[Unit], value: Token[Unit]): Either[EnvironmentError, Environment] = {
    val assignEither =
      if (values.contains(name.lexeme)) {
        Right(new Environment(values + (name.lexeme -> value), enclosing))
      } else {
        Left(EnvironmentError.UndefinedVariableError(name))
      }

    assignEither.recoverWith { case _ =>
      enclosing
        .map(
          _.assign(name, value)
            .map(newEnclosing => new Environment(values, Some(newEnclosing)))
        )
        .getOrElse(Left(EnvironmentError.UndefinedVariableError(name)))
    }
  }

}
