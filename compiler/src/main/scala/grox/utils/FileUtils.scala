package grox.utils

import cats.effect.{IO, Resource}
import java.io.{FileInputStream, File}
import scala.io.{Source, BufferedSource}

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
