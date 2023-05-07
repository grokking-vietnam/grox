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

/** Parses a sequence of `Token` into an `A`.
  *
  * The companion object provides standard parsers from which to start building larger ones. In particular, it contains
  * the necessary tools to start writing a string parser, such as [[char]] and [[string]].
  *
  * In order to provide better error messages, developers are encouraged to use [[label]] to describe the kind of thing
  * a parser will produce - a digit, for example, or an array, or...
  *
  * An important thing to realise is that parsers are non-backtracking by default. See the [[|]] documentation for
  * detailed information on the consequences of this design choice.
  */
trait Parser[Token, +A] {

  // - Main methods ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def parse[Source](input: Source)(implicit at: AsTokens[Source, Token], sm: SourceMap[Token]): Result[Token, A] = run(
    State.init(AsTokens[Source, Token].asTokens(input))
  )

  protected def run(state: State[Token]): Result[Token, A]

  // - Label handling --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Sets the label of this parser.
    *
    * A parser's label is a simple description of what they're expecting to parse. Typically, a parser who means to
    * parse a "true / false" value would have a `boolean` label.
    *
    * This allows us to provide meaningful error messages, where instead of saying "expected [, { or true", we can have
    * "expected array, object or boolean".
    */
  def label(label: String): Parser[Token, A] = state => run(state).label(label)

  // - Filtering -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Fails any parser that does not match the specified predicate.
    *
    * Note that such parsers are, of necessity, backtracking. Consider the following admitedly silly example:
    * {{{
    * val parser = digit.filter(_ != '9') | char('9')
    * parser.parse("9")
    * }}}
    *
    * If [[filter]] didn't turn [[digit]] backtracking, then this parser would fail, even though `9` is perfectly valid
    * input for it. [[digit]] would succeed and consume the `9`, then [[filter]] would fail the parser with a consuming
    * result - `char('9')` would not even be attempted.
    */
  def filter(f: A => Boolean): Parser[Token, A] = state =>
    run(state) match {
      case Result.Ok(_, parsed, _, msg) if !f(parsed.value) =>
        Result.Error(false, msg.copy(input = Message.Input.None))
      case other => other
    }

  def withFilter(f: A => Boolean): Parser[Token, A] = filter(f)

  def filterNot(f: A => Boolean): Parser[Token, A] = filter(f andThen (b => !b))

  /** A [[filter]] and a [[map]] rolled into one.
    *
    * For similar reasons to [[filter]], such parsers are backtracking. Consider the following example:
    * {{{
    * val parser = digit.collect {
    *   case c if c != '9' => c.toInt
    * } | char('9').as(9)
    *
    * parser.parse("9")
    * }}}
    *
    * If [[collect]] didn't turn the parser backtracking, this would fail, even though `9` is valid input: [[digit]]
    * would succeed and consume the `9`, then [[collect]] would fail the parser with a consuming result - `char('9')`
    * would not even be attempted.
    */
  def collect[B](f: PartialFunction[A, B]): Parser[Token, B] = state =>
    run(state) match {
      case Result.Ok(consumed, parsed, state, msg) =>
        f.lift(parsed.value) match {
          case Some(b) => Result.Ok(consumed, parsed.copy(value = b), state, msg)
          case None =>
            Result.Error(
              false,
              msg.copy(input = Message.Input.None, pos = parsed.start)
            )
        }
      case error: Result.Error[Token] => error
    }

  /** Fails when this parser succeeds, and succeeds when it fails.
    *
    * The returned parser will always be backtracking. Consider the following example:
    * {{{
    * val parser = (string("foo") <* !char('1')) ~ digit
    *
    * parser.parse("foo2")
    * }}}
    * If `!char('1')` was non-backtracking, then:
    *   - `2` would be consumed and confirmed to not be `1`.
    *   - the parser would then fail, because there is no digit to read.
    */
  def unary_! : Parser[Token, Unit] = {
    def negate(expected: List[String]) = expected.map(label => s"not $label")
    state =>
      void.run(state) match {
        case Result.Ok(_, parsed, _, msg) =>
          Result.Error(
            false,
            msg.copy(
              input = Message.Input.None,
              expected = negate(msg.expected)
            )
          )

        case Result.Error(_, message) =>
          Result.Ok(false, Parsed((), state.pos, state.pos), state, message)
      }
  }

  @inline def !~(p2: Parser[Token, Any]): Parser[Token, A]    = notFollowedBy(p2)
  def notFollowedBy(p2: Parser[Token, Any]): Parser[Token, A] = this <* !p2

  // - Mapping ---------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def map[B](f: A => B): Parser[Token, B] = state => run(state).map(f)

  def as[B](b: B): Parser[Token, B] = map(_ => b)

  def void: Parser[Token, Unit] = map(_ => ())

  def withPosition: Parser[Token, Parsed[A]] = state =>
    run(state) match {
      case Result.Ok(consumed, parsed, state, msg) => Result.Ok(consumed, parsed.map(_ => parsed), state, msg)
      case failure: Result.Error[Token]            => failure
    }

  // - Combining parsers -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def flatMap[B](f: A => Parser[Token, B]): Parser[Token, B] =
    state =>
      run(state) match {
        case Result.Ok(true, parsed, rest, _)  => f(parsed.value).run(rest).consume.setStart(parsed.start)
        case Result.Ok(false, parsed, rest, _) => f(parsed.value).run(rest).setStart(parsed.start)
        case error: Result.Error[Token]        => error
      }

  /** Attempts either this parser or the specified one.
    *
    * Note that this is non-backtracking: the alternative parser will only be tried if this parser is non-consuming.
    *
    * Consider the following example:
    * {{{
    * val parser =  string("foo") | string("bar")
    * parser.parse("foa")
    * }}}
    *
    * It's perfectly impossible for `bar` to be a valid match here, and we know that as soon as we've started
    * successfuly parsing `foo`. A non-backtracking parser will not attempt `bar`, which yields:
    *   - performance improvement (we're not trying parses that we know will fail).
    *   - better error messages (the error isn't that we were expecting `foo` or `bar`, but that we were parsing `foo`
    *     and found an `a` where we expected an `o`).
    *
    * It is sometimes necessary to override that behaviour. Take the following example:
    * {{{
    * val parser = string("foo") | string("foa")
    * parser.parse("foa")
    * }}}
    *
    * We want this to succeed, but since `string("foo")` is non-backtracking, `string("foa")` will not be attempted. In
    * these scenarios, calling [[backtrack]] on `string("foo")` allows `parser` to attempt `string("foa")`
    *
    * Finally, consider the following:
    * {{{
    * val parser = string("foo").backtrack | string("bar")
    * parser.parse("bar")
    * }}}
    *
    * This will succeed: non-consuming successes will still result in the alternative parser being attempted, to try and
    * find the first result that actually consumes data.
    */
  def |[AA >: A](p2: => Parser[Token, AA]): Parser[Token, AA] = state =>
    run(state).recoverWith { error1 =>
      if(error1.consumed) error1
      else
        p2.run(state).recoverWith { error2 =>
          if(error2.consumed) error2
          else error2.mapMessage(_.mergeExpected(error1.message))

        }
    }

  def orElse[AA >: A](p2: => Parser[Token, AA]): Parser[Token, AA] = this | p2

  def eitherOr[B](p2: Parser[Token, B]): Parser[Token, Either[B, A]] =
    map(Right.apply) | p2.map(Left.apply)

  def ~[B](p2: => Parser[Token, B]): Parser[Token, (A, B)] = for {
    a <- this
    b <- p2
  } yield (a, b)

  def *>[B](p2: => Parser[Token, B]): Parser[Token, B] = for {
    _ <- this
    b <- p2
  } yield b

  def <*[B](p2: => Parser[Token, B]): Parser[Token, A] = for {
    a <- this
    _ <- p2
  } yield a

  // - Misc. -----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  def surroundedBy(p: Parser[Token, Any]): Parser[Token, A] = between(p, p)

  def between(left: Parser[Token, Any], right: Parser[Token, Any]): Parser[Token, A] =
    left *> this <* right

  def backtrack: Parser[Token, A] = state => run(state).empty

  def ? : Parser[Token, Option[A]] = map(Option.apply) | Parser.pure(None)

  // - Repeating parsers -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  def rep0: Parser[Token, List[A]] =
    this.rep | Parser.pure(List.empty)

  def rep: Parser[Token, List[A]] =
    for {
      head <- this
      tail <- this.rep0
    } yield head +: tail

  def repSep[Sep](sep: Parser[Token, Sep]): Parser[Token, List[A]] =
    (this ~ (sep *> this).rep0).map { case (head, tail) => head :: tail }

  def repSep0[Sep](sep: Parser[Token, Sep]): Parser[Token, List[A]] =
    this.repSep(sep) | Parser.pure(List.empty)
}
// - Base parsers ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

object Parser {
  // - Common operations -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def pure[Token, A](value: A): Parser[Token, A] = state =>
    Result.Ok(false, Parsed(value, state.pos, state.pos), state, Message.empty)

  def ap[Token, A, B](ff: Parser[Token, A => B]): Parser[Token, A] => Parser[Token, B] = fa =>
    for {
      a <- fa
      f <- ff
    } yield f(a)

  // - Base parsers ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  def token[Token: SourceMap]: Parser[Token, Token] = TokenParser(_ => true)

  def satisfy[Token: SourceMap](f: Token => Boolean): Parser[Token, Token] =
    TokenParser(f)

  def end[Token: SourceMap]: Parser[Token, Unit] = state =>
    if(state.isEOF) Result.Ok(false, Parsed((), state.pos, state.pos), state, Message.empty)
    else Result.Error(false, Message(state, List("EOF")))

  def oneOf[Token, A](head: Parser[Token, A], tail: Parser[Token, A]*): Parser[Token, A] = tail.foldLeft(head)(_ | _)

  def sequence[Token, A](parsers: List[Parser[Token, A]]): Parser[Token, List[A]] = parsers match {
    case head :: tail =>
      for {
        h <- head
        t <- sequence(tail)
      } yield h :: t
    case Nil => pure(Nil)
  }

  // - Char parsers ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def char(c: Char): Parser[Char, Char]            = satisfy[Char](_ == c).label(c.toString)
  def char(f: Char => Boolean): Parser[Char, Char] = satisfy(f)
  def charIn(cs: Char*): Parser[Char, Char]        = charIn(cs)
  def charIn(cs: Iterable[Char]): Parser[Char, Char] = {
    val chars = cs.toSet

    satisfy(chars.contains)
  }

  private val letters = ('a' to 'z') ++ ('A' to 'Z')
  private val digits  = '0' to '9'

  val letter: Parser[Char, Char]     = charIn(letters).label("letter")
  val digit: Parser[Char, Char]      = charIn(digits).label("digit")
  val whitespace: Parser[Char, Char] = charIn(' ', '\t').label("whitespace")

  // - String parsers --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def string(s: String): Parser[Char, String] = new TokenListParser(s).as(s).label(s)

  val identifier: Parser[Char, String] =
    (charIn(letters :+ '_') ~ charIn(letters ++ digits :+ '_').rep0).map { case (head, tail) =>
      (head +: tail).mkString
    }
}
