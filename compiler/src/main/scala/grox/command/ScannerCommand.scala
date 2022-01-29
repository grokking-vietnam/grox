package grox.command

import cats.effect._
import cats.implicits._

import com.monovore.decline._
import grox.Scanner._
import grox._

object ScannerCommand {

  val pathOpts: Opts[String] = Opts.argument[String](metavar = "path")

  val scannerOpts: Opts[ScannerCommand] =
    Opts.subcommand("scanner", "Scan file to tokens") {
      pathOpts.map(ScannerCommand.apply)
    }

}

case class ScannerCommand(path: String)
