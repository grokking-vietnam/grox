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

import scala.annotation.tailrec

private class TokenListParser[Token: SourceMap](expected: IndexedSeq[Token]) extends Parser[Token, IndexedSeq[Token]] {

  protected def run(state: State[Token]): Result[Token, IndexedSeq[Token]] = {
    val left  = expected.iterator
    val right = state.input.iterator.drop(state.offset)

    def error(offset: Int, pos: Position, input: Message.Input[Token]) =
      Result.Error(offset != state.offset, Message(offset, pos, input, List.empty))

    @tailrec
    def loop(offset: Int, pos: Position): Result[Token, IndexedSeq[Token]] =
      if(left.hasNext) {
        if(right.hasNext) {
          val leftToken  = left.next()
          val rightToken = right.next()

          if(leftToken == rightToken) loop(offset + 1, SourceMap[Token].endsAt(rightToken, pos))
          else error(offset, pos, Message.Input.Token(rightToken))
        }
        else error(offset, pos, Message.Input.Eof)
      }
      else {
        val newState = state.copy(offset = offset, pos = pos)
        val parsed   = Parsed(expected, state.startsAt(state.input(state.offset)), newState.pos)
        Result.Ok(true, parsed, newState, Message.empty)
      }

    loop(state.offset, state.pos)
  }
}
