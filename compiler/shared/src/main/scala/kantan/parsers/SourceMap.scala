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

/** Type class used to keep track of a a token's position in a source file.
  *
  * A source map knows how to compute the following, given the current position in the input:
  *   - where the token starts.
  *   - where the token ends.
  *
  * In the case of characters, for example, the mapping is fairly straightforward. A character:
  *   - starts at the current position.
  *   - ends at the beginning of the following line if the character is a line break.
  *   - ends at the next column otherwise.
  *
  * One might imagine more complex scenarios, however. Typically, when splitting tokenization and parsing, you'll end up
  * working with tokens that know their position in the original source code.
  */
trait SourceMap[Token] {
  def endsAt(token: Token, current: Position): Position
  def startsAt(token: Token, current: Position): Position
}

// - Default instances -------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object SourceMap {

  def apply[Token](implicit sm: SourceMap[Token]): SourceMap[Token] = sm

  implicit val char: SourceMap[Char] = new SourceMap[Char] {
    def endsAt(token: Char, current: Position) =
      if(token == '\n') current.nextLine
      else current.nextColumn

    def startsAt(token: Char, current: Position) = current
  }
}
