package grox.utils

import cats.MonadThrow
import cats.syntax.all.*

trait FileReader[F[_]]:
  def read(path: String): F[String]

object FileReader:
  def instance[F[_]: MonadThrow]: FileReader[F] = path => FileUtils.read(path).liftTo[F]
