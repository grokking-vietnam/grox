package grox

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._

object Main
  extends CommandIOApp(
    name = "grox",
    header = "grox compiler",
    version = "0.0.1",
  ) {

  override def main: Opts[IO[ExitCode]] = {
    val helpOpts = Opts.flag("help", help = "Help String").orFalse

    helpOpts.map { helpOption =>
      for {
        _ <-
          if (helpOption) {
            IO.println("Help")
          } else {
            IO.println("Hello grox")
          }
        exitCode <- IO(ExitCode.Success)
      } yield exitCode

    }

  }

}
