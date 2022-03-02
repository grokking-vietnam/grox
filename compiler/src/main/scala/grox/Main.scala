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
      type F[A] = EitherT[IO, Scanner.Error, A]

      ScannerCommand.run[F](path).value.flatMap {
        case Right(e) => IO.pure(e)
        case Left(e)  => IO.println("Unknown Error").map(_ => ExitCode.Error)
      }

    // val fileUtil = new ExtendFileUtils[IO]
    // // type ScannerResult[A] = Either[Scanner.Error, A]
    // val scanner: Scanner[Either[Scanner.Error, *]] = Scanner.instance

    // def scannerIO(content: String) = EitherT.fromEither[IO](
    //   scanner.parse(content)
    // )

    // (for {
    //   content <- EitherT(
    //     fileUtil
    //       .open(path)
    //       .use { buffer =>
    //         fileUtil.read(buffer)
    //       }
    //       .attempt
    //   ).leftMap(grox.FileError.apply)
    //   tokens <- scannerIO(content)
    //   _ <- EitherT.liftF(IO.println(tokens))

    // } yield tokens).value.flatMap {
    //   case Right(_)                => IO.pure(ExitCode.Success)
    //   case Left(err: FileError)    => IO.println("Couldn't open file").map(_ => ExitCode.Error)
    //   case Left(err: ScannerError) => IO.println("Couldn't scan file").map(_ => ExitCode.Error)
    //   case Left(_)                 => IO.println("Unknown Error").map(_ => ExitCode.Error)

    // }
    }

}
