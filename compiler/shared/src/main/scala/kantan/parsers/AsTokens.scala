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

/** Type class that describes the capacity to tokenize something.
  *
  * A tokenized value is an indexed sequence (ideally an array). This allows the parser to navigate input not by
  * consuming it bit by bit and constructing tons of intermediate representations, but as a pointer in something
  * array-ish.
  *
  * Strings, for example, can be turned into an array of characters at very little cost.
  */
trait AsTokens[Source, Token] {
  def asTokens(source: Source): IndexedSeq[Token]
}

object AsTokens {
  def apply[Source, Token](implicit at: AsTokens[Source, Token]): AsTokens[Source, Token] = at

  /** Strings tokenize to arrays of characters. */
  implicit val string: AsTokens[String, Char] = source => source.toArray.toIndexedSeq

  /** Indexed sequences are already tokenized. */
  implicit def indexedSeq[Token]: AsTokens[IndexedSeq[Token], Token] = source => source

  implicit def seq[Col[X] <: Seq[X], Token]: AsTokens[Col[Token], Token] = source => source.toIndexedSeq
}
