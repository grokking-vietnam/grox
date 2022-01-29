package grox.utils

import java.io.{File, FileInputStream}

import scala.io.{BufferedSource, Source}

import cats.effect.{IO, Resource}

object FileUtils {

  def open(path: String): Resource[IO, BufferedSource] =
    Resource.make {
      IO.blocking(Source.fromFile(path))
    } { buffer =>
      IO.blocking(
        buffer.close()
      ).handleErrorWith(_ => IO.unit)
    }

}
