package grox.command

import com.monovore.decline._
import grox.Scanner._
import grox._
import cats.effect._
import cats.implicits._

object ScannerCommand {

  case class ThrowableError(error: Error) extends Exception

  def scan(content: String): IO[List[Token]] =
    for {
      c <- IO.pure(content)
      tokens <- IO.fromEither(Scanner.parse(c).leftMap(ThrowableError.apply))

    } yield tokens

  val pathOpts: Opts[String] = Opts.argument[String](metavar = "path")

  val scannerOpts: Opts[ScannerCommand] =
    Opts.subcommand("scanner", "Scan file to tokens") {
      pathOpts.map(ScannerCommand.apply)
    }

}

case class ScannerCommand(path: String)
