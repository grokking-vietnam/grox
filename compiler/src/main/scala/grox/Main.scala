package grox

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._
import grox.command.ScannerCommand
import grox.utils.FileUtils
import grox.Scanner

object Main
  extends CommandIOApp(
    name = "grox",
    header = "grox compiler",
    version = "0.0.1",
  ) {

  override def main: Opts[IO[ExitCode]] = ScannerCommand
    .scannerOpts
    .map { case ScannerCommand(path) =>
      for {
        content <- FileUtils.open(path).use { buffer =>
          IO(buffer.getLines.mkString)
        }
        tokens <- ScannerCommand.scan(content)
        _ <- IO.println(tokens)

      } yield (ExitCode.Success)
    }

}
