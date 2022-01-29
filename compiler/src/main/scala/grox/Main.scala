package grox

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._
import grox.command.ScannerCommand
import grox.utils.FileUtils
import grox.Scanner
import grox._
import cats.data.EitherT
import javax.management.InstanceAlreadyExistsException

object Main
  extends CommandIOApp(
    name = "grox",
    header = "grox compiler",
    version = "0.0.1",
  ) {

  override def main: Opts[IO[ExitCode]] = ScannerCommand
    .scannerOpts
    .map { case ScannerCommand(path) =>
      (for {
        content <- EitherT(
          FileUtils
            .open(path)
            .use { buffer =>
              IO(buffer.getLines.mkString)
            }
            .attempt
        ).leftMap(grox.FileError.apply)
        tokens <- EitherT
          .fromEither[IO](Scanner.parse(content))
          .leftMap(grox.ScannerError.apply)
        _ <- EitherT.liftF(IO.println(tokens))

      } yield tokens).value.flatMap {
        case Right(_)                => IO.pure(ExitCode.Success)
        case Left(err: FileError)    => IO.println("Couldn't open file").map(_ => ExitCode.Error)
        case Left(err: ScannerError) => IO.println("Couldn't scan file").map(_ => ExitCode.Error)

      }
    }

}
