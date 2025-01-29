package grox

import scala.util.control.NoStackTrace

import cats.implicits.catsSyntaxEither

object State:
  def apply(): State = State(Map.empty[String, LiteralType], enclosing = None)

enum StateError(val msg: String) extends NoStackTrace:
  case UndefinedVariableError(variable: String)
    extends StateError(s"Undefined variable: '$variable'.")

case class State(
  val values: Map[String, LiteralType] = Map.empty[String, LiteralType],
  val enclosing: Option[State] = None,
):

  def define(
    name: String,
    value: LiteralType,
  ): State = State(values + (name -> value), enclosing)

  def get(
    name: String
  ): Either[StateError, LiteralType] = _get(name)
    .toRight(StateError.UndefinedVariableError(name))

  private def _get(
    name: String
  ): Option[LiteralType] = values
    .get(name)
    .orElse(enclosing.flatMap(_._get(name)))

  def assign(name: String, value: LiteralType): Either[StateError, State] =
    val assignEither =
      if values.contains(name) then Right(State(values + (name -> value), enclosing))
      else Left(StateError.UndefinedVariableError(name))

    assignEither.recoverWith { case _ =>
      enclosing
        .map(
          _.assign(name, value)
            .map(newEnclosing => State(values, Some(newEnclosing)))
        )
        .getOrElse(Left(StateError.UndefinedVariableError(name)))
    }
