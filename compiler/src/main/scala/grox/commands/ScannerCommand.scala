package grox.commands

import cats.*
import cats.data.*
import cats.effect.*
import cats.effect.std.*
import cats.implicits.*

import com.monovore.decline.*
import grox.Scanner.*
import grox.Token
import grox.utils.FileUtils.FileAgl
import grox.utils.FileUtils.implicits.FileUtil

object ScannerCommand {

  case class Config(path: String, verbose: Boolean)

  val pathOpts: Opts[String] = Opts.argument[String](metavar = "path")
  val verboseOpts = Opts.flag("verbose", help = "Show verbose error").orFalse

  val scannerOpts: Opts[Config] =
    Opts.subcommand("scanner", "Scan file to tokens") {
      (pathOpts, verboseOpts)
        .mapN((path, verbose) => Config(path, verbose))
    }

  def run(cfg: Config): IO[ExitCode] = {

    type F[A] = EitherT[IO, grox.Error, A]

    (scan[F].run(cfg.path).value flatMap {
      case Right(e) => IO.println(e) >> IO.pure(ExitCode.Success)
      case Left(e)  => IO.println(s"Error $e").map(_ => ExitCode.Error)
    }).recoverWith { e =>
      IO.println(s"Error $e").map(_ => ExitCode.Error)
    }
  }

  trait ADT[F[_]] {
    def run(path: String): F[List[Token]]
  }

  def scan[F[_]](
    using FU: FileAgl[F],
    S: ScannerAgl[F],
    M: MonadError[F, grox.Error],
  ): ADT[F] =
    new ADT[F] {

      def run(path: String): F[List[Token]] =
        for {
          content <- FU.read(path)
          tokens <- S.scan(content)
        } yield tokens

    }

}
