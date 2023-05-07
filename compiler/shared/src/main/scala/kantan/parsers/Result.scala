/*
 * Copyright 2022 Nicolas Rinaudo
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package kantan.parsers

/** Result of a parsing operation.
  *
  * This is essentially a very specialised version of `Either` (and can, in fact, be turned into one through
  * [[toEither]]).
  *
  * A result keeps track of whether or not any data has been consumed when producing it. This is used to decide whether
  * or not to try alternative parsers in a [[Parser.|]] call.
  *
  * Results also store an error message *even if they're successful*. This might seem a little odd, but is necessary to
  * be able to provide good error messages for combinators such as [[Parser.filter]] where we might turn a success into
  * a failure after the fact.
  */
sealed trait Result[Token, +A] {
  def recoverWith[AA >: A](f: Result.Error[Token] => Result[Token, AA]): Result[Token, AA] = this match {
    case ok: Result.Ok[Token, A]      => ok
    case failure: Result.Error[Token] => f(failure)
  }

  def toEither: Either[Message[Token], A] = this match {
    case Result.Ok(_, parsed, _, _) => Right(parsed.value)
    case Result.Error(_, msg)       => Left(msg)
  }

  def setStart(pos: Position): Result[Token, A] = this match {
    case ok: Result.Ok[Token, A] => ok.copy(value = ok.value.copy(start = pos))
    case other                   => other
  }

// - Common properties -----------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------
  def consumed: Boolean
  def message: Message[Token]

// - Label handling --------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------
  def label(label: String): Result[Token, A] = mapMessage(_.expecting(label))

// - Mapping ---------------------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------
  def mapMessage(f: Message[Token] => Message[Token]): Result[Token, A] = this match {
    case ok: Result.Ok[Token, A]    => ok.copy(message = f(message))
    case error: Result.Error[Token] => error.copy(message = f(message))
  }

  def map[B](f: A => B): Result[Token, B] = this match {
    case ok: Result.Ok[Token, A] => ok.copy(value = ok.value.map(f))
    case e: Result.Error[Token]  => e
  }

// - Backtrack handling ----------------------------------------------------------------------------------------------
// -------------------------------------------------------------------------------------------------------------------
  /** Marks this result as consuming. */
  def consume: Result[Token, A] = this match {
    case ok: Result.Ok[Token, A]  => ok.copy(consumed = true)
    case err: Result.Error[Token] => err.copy(consumed = true)
  }

  /** Marks this result as non-consuming. */
  def empty: Result[Token, A] = this match {
    case ok: Result.Ok[Token, A]  => ok.copy(consumed = false)
    case err: Result.Error[Token] => err.copy(consumed = false)
  }

}

object Result {
  final case class Ok[Token, A](consumed: Boolean, value: Parsed[A], state: State[Token], message: Message[Token])
      extends Result[Token, A]
  final case class Error[Token](consumed: Boolean, message: Message[Token]) extends Result[Token, Nothing]
}
