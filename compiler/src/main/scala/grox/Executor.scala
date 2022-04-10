package grox

import cats.MonadThrow
import cats.implicits.*

trait Executor[F[_]]:
  def scan(str: String): F[List[Token]]
  def parse(str: String): F[Expr]

object Executor:

  def instance[F[_]: MonadThrow](
    using scanner: Scanner[F],
    parser: Parser[F],
  ): Executor[F] =
    new Executor[F]:
      def scan(str: String): F[List[Token]] = scanner.scan(str)

      def parse(str: String): F[Expr] =
        for
          tokens <- scanner.scan(str)
          expr <- parser.parse(tokens)
        yield expr

  def module[F[_]: MonadThrow]: Executor[F] =
    given Scanner[F] = Scanner.instance[F]
    given Parser[F] = Parser.instance[F]
    instance[F]
