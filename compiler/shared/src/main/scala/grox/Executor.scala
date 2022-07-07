package grox

import cats.MonadThrow
import cats.implicits.*
import cats.mtl.Stateful

trait Executor[F[_]]:
  def scan(str: String): F[List[Token[Span]]]
  def parse(str: String): F[Expr]
  def evaluate(str: String): F[LiteralType]

object Executor:

  def instance[F[_]: MonadThrow](
    using scanner: Scanner[F],
    parser: Parser[F],
    interpreter: Interpreter[F],
  ): Executor[F] =
    new Executor[F]:
      def scan(str: String): F[List[Token[Span]]] = scanner.scan(str)

      def parse(str: String): F[Expr] =
        for
          tokens <- scanner.scan(str)
          expr <- parser.parse(tokens)
        yield expr

      def evaluate(str: String): F[LiteralType] =
        for
          tokens <- scanner.scan(str)
          expr <- parser.parse(tokens)
          result <- interpreter.evaluate(expr)
        yield result

  def module[F[_]: MonadThrow](using S: Stateful[F, Environment]): Executor[F] =
    given Scanner[F] = Scanner.instance[F]
    given Parser[F] = Parser.instance[F]
    given Interpreter[F] = Interpreter.instance[F]
    instance[F]
