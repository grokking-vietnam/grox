package grox

import cats.implicits.catsSyntaxEither

object Environment:
  def apply(): Environment = new Environment(Map.empty[String, LiteralType], enclosing = None)

enum EnvironmentError(msg: String):
  case UndefinedVariableError(variable: String)
    extends EnvironmentError(s"Undefined variable: '$variable'.")

class Environment(
  private val values: Map[String, LiteralType],
  private val enclosing: Option[Environment],
):

  def define(name: String, value: LiteralType): Environment =
    new Environment(values + (name -> value), enclosing)

  def get(
    name: String
  ): Either[EnvironmentError, LiteralType] = _get(name)
    .toRight(EnvironmentError.UndefinedVariableError(name))

  private def _get(
    name: String
  ): Option[LiteralType] = values
    .get(name)
    .orElse(enclosing.flatMap(_._get(name)))

  def assign(name: String, value: LiteralType): Either[EnvironmentError, Environment] =
    val assignEither =
      if (values.contains(name))
        Right(new Environment(values + (name -> value), enclosing))
      else
        Left(EnvironmentError.UndefinedVariableError(name))

    assignEither.recoverWith { case _ =>
      enclosing
        .map(
          _.assign(name, value)
            .map(newEnclosing => new Environment(values, Some(newEnclosing)))
        )
        .getOrElse(Left(EnvironmentError.UndefinedVariableError(name)))
    }
