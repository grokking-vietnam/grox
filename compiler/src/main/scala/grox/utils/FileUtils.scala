package grox.utils

import java.io.{File, FileInputStream}

import scala.io.{BufferedSource, Source}

import cats._
import cats.effect.kernel.Sync
import cats.effect.{IO, Resource}
import cats.implicits._

import grox._

object FileUtils {

  def open[F[_]](
    path: String
  )(
    implicit Sync: Sync[F]
  ): Resource[F, BufferedSource] =
    Resource.make {
      Sync.blocking(Source.fromFile(path))
    } { buffer =>
      Sync.blocking(
        buffer.close()
      )
    }

  def read[F[_]](
    f: BufferedSource
  )(
    implicit Sync: Sync[F],
    ME: MonadError[F, Throwable],
  ): F[String] = Sync.blocking(f.getLines.mkString).attempt.flatMap {
    case Right(r) => Sync.pure(r)
    case Left(e)  => ME.raiseError(e)
  }

}
