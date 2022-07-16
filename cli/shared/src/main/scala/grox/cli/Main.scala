package grox.cli

import cats.Functor
import cats.effect.*
import cats.syntax.all.*

import com.monovore.decline.*
import com.monovore.decline.effect.*
import grox.Executor

object Main
  extends CommandIOApp(
    name = "grox",
    header = "grox compiler",
    version = "0.0.1",
  ):

  enum Command:
    case Scan(file: String)
    case Parse(file: String)
    case Evaluate(file: String)

  def convertCommand[F[_]: Functor](using reader: FileReader[F]): CLI.Command => F[Command] =
    case CLI.Command.Scan(file)     => reader.read(file).map(Command.Scan(_))
    case CLI.Command.Parse(file)    => reader.read(file).map(Command.Parse(_))
    case CLI.Command.Evaluate(file) => reader.read(file).map(Command.Evaluate(_))

  def eval[F[_]: Functor](exec: Executor[F]): Command => F[String] =
    case Command.Scan(str)     => exec.scan(str).map(tokens => tokens.mkString("\n"))
    case Command.Parse(str)    => exec.parse(str).map(_.show)
    case Command.Evaluate(str) => exec.evaluate(str).map(_.toString)

  given FileReader[IO] = FileReader.instance[IO]

  override def main: Opts[IO[ExitCode]] = CLI.parse.map { command =>
    Executor
      .module[IO]
      .use(exec =>
        convertCommand[IO](command)
          .flatMap { cmd =>
            eval(exec)(cmd)
          }
          .flatMap(IO.println)
          .handleErrorWith { err =>
            IO.println(s"Error: ${err.toString}")
          }
      )
      .as(ExitCode.Success)
  }
