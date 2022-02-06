package grox.utils

import java.io.{File, FileInputStream}

import scala.io.{BufferedSource, Source}

import cats.effect.{IO, Resource}
import cats.effect.kernel.Sync

trait FileUtils[F[_]] {
  def open(path: String): Resource[F, BufferedSource]

  def read(f: BufferedSource): F[String]

}

class ExtendFileUtils[F[_]: Sync] extends FileUtils[F] {

  override def open(path: String): Resource[F, BufferedSource] =
    Resource.make {
      Sync[F].blocking(Source.fromFile(path))
    } { buffer =>
      Sync[F].blocking(
        buffer.close()
      )
    }

  override def read(f: BufferedSource): F[String] = Sync[F].blocking(f.getLines.mkString)

}
