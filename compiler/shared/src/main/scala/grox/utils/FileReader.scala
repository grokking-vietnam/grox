package grox.utils

import cats.MonadThrow
import cats.syntax.all.*

import fs2.io.file.Files

trait FileReader[F[_]]:
  def read(path: String): F[String]

object FileReader:

  // def instance[F[_]: MonadThrow]: FileReader[F] = path => FileUtils.read(path).liftTo[F]

  def instance[F[_]: Files: MonadThrow](using fs2.Compiler[F, F]): FileReader[F] =
    path =>
      Files[F]
        .readAll(fs2.io.file.Path(path))
        .through(fs2.text.utf8.decode[F])
        .compile
        .string
        .handleErrorWith(_ => Left(grox.Error.FileNotFound(path)).liftTo[F])
