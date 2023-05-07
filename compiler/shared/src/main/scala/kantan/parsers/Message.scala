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

/** Parser error message.
  *
  * An error message contains:
  *   - the index of the token at which the error was encountered.
  *   - the position (line and column) at which the error was encountered.
  *   - the token that cause the failure, as a string.
  *   - a list of the values that were expected.
  */
final case class Message[Token](offset: Int, pos: Position, input: Message.Input[Token], expected: List[String]) {
  def expecting(label: String): Message[Token]             = copy(expected = List(label))
  def mergeExpected(other: Message[Token]): Message[Token] = copy(expected = expected ++ other.expected)
}

object Message {
  // - Input that triggered the message --------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  sealed trait Input[+A] extends Product with Serializable

  object Input {

    /** No known input was consumed.
      *
      * At the time of writing, this can only occur in two scenarios:
      *   - the parser failed without parsing input which... not sure this is an actual possibility.
      *   - the failure wasn't triggered by an unexpected token, but by filtering out successful parses.
      */
    final case object None extends Input[Nothing]

    /** We reached the end of the file. */
    final case object Eof extends Input[Nothing]

    /** Token from the input stream. */
    final case class Token[A](value: A) extends Input[A]
  }

  // - Construction ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def empty[Token]: Message[Token] = Message(0, Position.zero, Input.None, List.empty)

  def apply[Token: SourceMap](state: State[Token], expected: List[String]): Message[Token] =
    if(state.isEOF) Message(state.offset, state.pos, Input.Eof, expected)
    else {
      val token = state.input(state.offset)
      Message(state.offset, SourceMap[Token].startsAt(token, state.pos), Input.Token(token), expected)
    }
}
