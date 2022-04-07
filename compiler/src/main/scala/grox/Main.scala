package grox

import cats.data.EitherT
import cats.effect.*
import cats.implicits.*
import cats.{Functor, Show}

import com.monovore.decline.*
import com.monovore.decline.effect.*
import grox.cli.CLI
import grox.utils.FileAgl

object Main
  extends CommandIOApp(
    name = "grox",
    header = "grox compiler",
    version = "0.0.1",
  ) {

  enum Command:
    case Scan(file: String)
    case Parse(file: String)

  def convertCommand[F[_]: FileAgl: Functor]: CLI.Command => F[Command] =
    case CLI.Command.Scan(file)  => FileAgl[F].read(file).map(Command.Scan(_))
    case CLI.Command.Parse(file) => FileAgl[F].read(file).map(Command.Parse(_))

  def eval[F[_]: Functor](exec: Executor[F]): Command => F[String] =
    case Command.Scan(str) =>
      exec.scan(str).map { tokens =>
        tokens.mkString("\n")
      }
    case Command.Parse(str) => exec.parse(str).map(_.show)

  given FileAgl[IO] = FileAgl.instance[IO]
  val exec = Executor.module[IO]

  override def main: Opts[IO[ExitCode]] = CLI.parse.map {
    convertCommand[IO](_)
      .flatMap { cmd =>
        eval(exec)(cmd)
      }
      .flatMap(IO.println)
      .as(ExitCode.Success)
  }

}
