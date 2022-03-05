package grox

import javax.management.InstanceAlreadyExistsException

import cats._
import cats.data.EitherT
import cats.effect.std._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import cats.syntax.all._

import com.monovore.decline._
import com.monovore.decline.effect._
import grox._
import grox.command.ScannerCommand
import grox.utils._

object Main
  extends CommandIOApp(
    name = "grox",
    header = "grox compiler",
    version = "0.0.1",
  ) {

  override def main: Opts[IO[ExitCode]] = ScannerCommand
    .scannerOpts
    .map { case ScannerCommand(path) =>
      type F[A] = EitherT[IO, grox.Error, A]

      ScannerCommand.run[F](path).value.flatMap {
        case Right(e) => IO.pure(e)
        case Left(e)  => IO.println(s"Error $e").map(_ => ExitCode.Error)
      }

    }

}
