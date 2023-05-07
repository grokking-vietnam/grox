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

/** Represents a position in a source file.
  *
  * This is supposed to work in conjunction with [[SourceMap]], to allow a parser to automatically keep track of where
  * in a source file a token was encountered.
  */
final case class Position(line: Int, column: Int) {
  def nextLine: Position   = Position(line + 1, 0)
  def nextColumn: Position = Position(line, column + 1)
}

object Position {
  val zero: Position = Position(0, 0)
}
