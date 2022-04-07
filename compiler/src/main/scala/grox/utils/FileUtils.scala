package grox.utils

import java.nio.file.Path

import scala.io.Source
import scala.util.{Failure, Success, Try}

import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import cats.{Applicative, MonadError, MonadThrow}

trait FileAgl[F[_]]:
  def read(path: Path): F[String]

object FileAgl:
  def apply[F[_]](using F: FileAgl[F]): FileAgl[F] = F

  def instance[F[_]: MonadThrow]: FileAgl[F] =
    path =>
      Try {
        val bufferedSource = Source.fromFile(path.toString)
        val content = bufferedSource.getLines.mkString
        bufferedSource.close
        content
      }.toEither.leftMap(_ => grox.Error.UnexpectedError).liftTo[F]

// given AsyncFileUtil[F[_]](
// using
// A: Applicative[F],
// Sync: Sync[F],
// ): FileAgl[F] =
// new FileAgl[F] {
// def read(path: String): F[String] = Resource
// .make {
// Sync.blocking(Source.fromFile(path))
// } { buffer =>
// Sync.blocking {
// buffer.close()
// }
// }
// .use { f =>
// Sync.blocking(f.getLines.mkString)
// }
// }
