package grox.command

import com.monovore.decline._
import grox.Scanner._
import grox._
import cats.effect._
import cats.implicits._

object ScannerCommand {

  case class ThrowableError(error: Error) extends Exception

  def scan(content: String): Either[Error, List[Token]] = {
    Scanner.parse(content)
  }

  val pathOpts: Opts[String] = Opts.argument[String](metavar = "path")

  val scannerOpts: Opts[ScannerCommand] =
    Opts.subcommand("scanner", "Scan file to tokens") {
      pathOpts.map(ScannerCommand.apply)
    }

}

case class ScannerCommand(path: String)
