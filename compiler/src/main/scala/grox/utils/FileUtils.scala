package grox.utils

import scala.io.Source
import scala.util.{Failure, Success, Try}

import cats.effect.*
import cats.effect.implicits.*
import cats.implicits.*
import cats.{Applicative, MonadError}

object FileUtils {

  trait FileAgl[F[_]] {
    def read(path: String): F[String]
  }

  object implicits {

    given FileUtil[F[_]](
      using ME: MonadError[F, grox.Error],
      A: Applicative[F],
    ): FileAgl[F] =
      new FileAgl[F] {

        def read(path: String): F[String] =
          Try {
            val bufferedSource = Source.fromFile(path)
            val content = bufferedSource.getLines.mkString
            bufferedSource.close
            content
          } match {
            case Success(v) => A.pure(v)
            case Failure(_) => ME.raiseError(grox.Error.UnexpectedError)
          }

      }

    given AsyncFileUtil[F[_]](
      using
      A: Applicative[F],
      Sync: Sync[F],
    ): FileAgl[F] =
      new FileAgl[F] {

        def read(path: String): F[String] = Resource
          .make {
            Sync.blocking(Source.fromFile(path))
          } { buffer =>
            Sync.blocking {
              buffer.close()
            }
          }
          .use { f =>
            Sync.blocking(f.getLines.mkString)
          }

      }

  }

}
