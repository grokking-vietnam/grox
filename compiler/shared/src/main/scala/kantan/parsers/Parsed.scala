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

/** Parsed value, equiped with its start and end position in the original source code. */
final case class Parsed[+A](value: A, start: Position, end: Position) {
  def map[B](f: A => B): Parsed[B]        = copy(value = f(value))
  def withStart(pos: Position): Parsed[A] = copy(start = pos)
}
