package grox

import scala.util.control.NoStackTrace

import cats.implicits.catsSyntaxEither

object Environment:
  def apply(): Environment = Environment(Map.empty[String, LiteralType], enclosing = None)

enum EnvironmentError(msg: String) extends NoStackTrace:
  case UndefinedVariableError(variable: String)
    extends EnvironmentError(s"Undefined variable: '$variable'.")

case class Environment(
  val values: Map[String, LiteralType] = Map.empty[String, LiteralType],
  val enclosing: Option[Environment] = None,
):

  def define(
    name: String,
    value: LiteralType,
  ): Environment = Environment(values + (name -> value), enclosing)

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
        Right(Environment(values + (name -> value), enclosing))
      else
        Left(EnvironmentError.UndefinedVariableError(name))

    assignEither.recoverWith { case _ =>
      enclosing
        .map(
          _.assign(name, value)
            .map(newEnclosing => Environment(values, Some(newEnclosing)))
        )
        .getOrElse(Left(EnvironmentError.UndefinedVariableError(name)))
    }
