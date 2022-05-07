package grox.utils

import java.nio.file.Path

import scala.io.Source
import scala.util.{Failure, Success, Try}

import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import cats.{Applicative, MonadError, MonadThrow}

trait FileReader[F[_]]:
  def read(path: Path): F[String]

object FileReader:

  def instance[F[_]: MonadThrow]: FileReader[F] =
    path =>
      Try {
        val bufferedSource = Source.fromFile(path.toString)
        val content = bufferedSource.getLines.mkString
        bufferedSource.close
        content
      }.toEither.leftMap(_ => grox.Error.FileNotFound(path.toString)).liftTo[F]
