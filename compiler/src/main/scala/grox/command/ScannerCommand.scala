package grox.command

import cats._
import cats.effect._
import cats.effect.std.Console
import cats.implicits._
import cats.mtl._
import cats.mtl.implicits._

import com.monovore.decline._
import grox.Scanner._
import grox._
import grox.utils.FileUtils

object ScannerCommand {

  val pathOpts: Opts[String] = Opts.argument[String](metavar = "path")

  val scannerOpts: Opts[ScannerCommand] =
    Opts.subcommand("scanner", "Scan file to tokens") {
      pathOpts.map(ScannerCommand.apply)
    }

  def run[F[_]](
    path: String
  )(
    implicit Sync: Sync[F],
    ME: MonadError[F, Throwable],
    AE: Handle[F, Scanner.Error],
    CS: Console[F],
  ): F[cats.effect.ExitCode] =
    (for {
      content <- FileUtils.open(path).use { buffer =>
        FileUtils.read(buffer)
      }
      tokens <- Scanner.instance.parse(content)
      _ <- CS.println(tokens)
    } yield tokens)
      .map(tokens => ExitCode.Success)
      .handleWith[Scanner.Error] { case e: Scanner.Error => Sync.pure(ExitCode.Error) }
      .recover { case exc: Throwable =>
        ExitCode.Error
      }

}

case class ScannerCommand(path: String)
