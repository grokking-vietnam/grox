package grox

import cats.data.EitherT
import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._
import grox.commands._

object Main
  extends CommandIOApp(
    name = "grox",
    header = "grox compiler",
    version = "0.0.1",
  ) {

  override def main: Opts[IO[ExitCode]] =
    (ScannerCommand.scannerOpts orElse ParseCommand.parserOpts)
      .map {
        case cfg: ScannerCommand.Config => ScannerCommand.run(cfg)
        case cfg: ParseCommand.Config   => ParseCommand.run(cfg)
      }

}
