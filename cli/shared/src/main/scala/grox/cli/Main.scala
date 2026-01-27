package grox.cli

import cats.Functor
import cats.effect.*
import cats.effect.std.Console
import cats.syntax.all.*

import com.monovore.decline.*
import com.monovore.decline.effect.*
import grox.Executor
import scribe.cats.*
import scribe.{Level, Logger}

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
    case Execute(file: String)

  def convertCommand[F[_]: Functor](
    using reader: FileReader[F]
  ): CLI.Command => F[Command] =
    case CLI.Command.Scan(file)     => reader.read(file).map(Command.Scan(_))
    case CLI.Command.Parse(file)    => reader.read(file).map(Command.Parse(_))
    case CLI.Command.Evaluate(file) => reader.read(file).map(Command.Evaluate(_))
    case CLI.Command.Run(file)      => reader.read(file).map(Command.Execute(_))

  def eval[F[_]: {Console, Concurrent}](exec: Executor[F]): Command => F[String] =
    case Command.Scan(str)     => exec.scan(str).map(tokens => tokens.mkString("\n"))
    case Command.Parse(str)    => exec.parse(str).map(_.show)
    case Command.Evaluate(str) => exec.evaluate(str).map(_.toString)
    case Command.Execute(str)  =>
      exec.execute(str).evalMap(Console[F].println).compile.drain.as("Done")

  given FileReader[IO] = FileReader.instance[IO]
  val exec = Executor.module[IO]

  override def main: Opts[IO[ExitCode]] = CLI
    .parse
    .map(config =>
      val level = if config.debug then Level.Debug else Level.Error
      val _ = Logger.root.clearHandlers().withHandler(minimumLevel = Some(level)).replace()
      Executor
        .module[IO]
        .use(exec =>
          convertCommand[IO](config.command)
            .flatMap { cmd =>
              eval(exec)(cmd)
            }
            .flatMap(IO.println)
            .handleErrorWith { err =>
              IO.println(s"Error: ${err.toString}")
            }
        )
        .as(ExitCode.Success)
    )
