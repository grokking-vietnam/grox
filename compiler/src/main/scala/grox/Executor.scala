package grox

import cats.MonadThrow
import cats.implicits.*

trait Executor[F[_]]:
  def scan(str: String): F[List[Token]]
  def parse(str: String): F[Expr]

object Executor:
  def apply[F[_]](using F: Executor[F]): Executor[F] = F

  def instance[F[_]: ScannerAgl: ParserAgl: MonadThrow]: Executor[F] =
    new Executor[F]:
      def scan(str: String): F[List[Token]] = Scanner[F].scan(str)

      def parse(str: String): F[Expr] =
        for
          tokens <- Scanner[F].scan(str)
          expr <- Parser[F].parse(tokens)
        yield expr

  def module[F[_]: MonadThrow]: Executor[F] =
    given ScannerAgl[F] = Scanner.instance[F]
    given ParserAgl[F] = Parser.instance[F]
    instance[F]
