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

/** Optimised version of [[Parser]] when working at the token level.
  *
  * Knowing that we're working directly with the input allows us, for example, to treat repetition operators as ranging
  * over an array rather than the default step-by-step approach.
  *
  * For example:
  * {{{
  * val parser = digit.rep
  * parser.parse("567")
  * }}}
  *
  * [[TokenParser]] will treat this as extracting the sub-sequence from index 0 to 3 in `567`, rather than parsing `5`,
  * `6` and `7` individually, and accumulate them in a list.
  */
private class TokenParser[Token: SourceMap](parse: State[Token] => Result[Token, Token], pred: Token => Boolean)
    extends Parser[Token, Token] {

  // - Parser methods --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def run(state: State[Token]): Result[Token, Token] = parse(state)

  override def label(label: String): TokenParser[Token] = new TokenParser(state => run(state).label(label), pred)
  override def backtrack                                = new TokenParser(state => run(state).empty, pred)

  // - Repetition ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def rep(allowEmpty: Boolean): Parser[Token, List[Token]] = state => {
    val start = state.offset
    val stop = {
      val index = state.input.indexWhere(token => !pred(token), start)
      if(index < 0) state.input.length
      else index
    }

    if(start < stop) {
      val value    = state.input.slice(start, stop).toList
      val newState = state.consumeRep(value)

      // There's an easy to miss trap here: the start position of the value we've just parsed is not necessarily
      // the current position in the input. If, for example, we're working with non-contiguous tokens, the current
      // position represents where the previous token ends, which is not quite the same thing as where the new token
      // begins.
      val parsed = Parsed(value, state.startsAt(state.input(start)), newState.pos)

      Result.Ok(true, parsed, newState, Message.empty)
    }
    else if(allowEmpty) Result.Ok(false, Parsed(List.empty, state.pos, state.pos), state, Message.empty)
    else Result.Error(false, Message(state, List.empty))
  }

  override def rep = rep(false)

  override def rep0 = rep(true)
}

private[parsers] object TokenParser {
  // - Basic parser ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  def apply[Token: SourceMap](pred: Token => Boolean): TokenParser[Token] = new TokenParser(
    state =>
      if(state.isEOF) Result.Error(false, Message(state, List.empty))
      else {
        val value = state.input(state.offset)

        if(pred(value)) {
          val newState = state.consume(value)
          val parsed   = Parsed(value, state.startsAt(value), newState.pos)

          Result.Ok(true, parsed, newState, Message.empty)
        }
        else Result.Error(false, Message(state, List.empty))
      },
    pred
  )
}
