package grox.utils

import scala.io.Source
import scala.util.{Failure, Success, Try}

import cats.effect.implicits.*
import cats.implicits.*
import cats.{Applicative, MonadError, MonadThrow}

object FileUtils:
  def read(path: String) : Either[grox.Error, String] =
      Try {
        val bufferedSource = Source.fromFile(path)
        val content = bufferedSource.getLines.mkString
        bufferedSource.close
        content
      }.toEither.leftMap(_ => grox.Error.FileNotFound(path))
