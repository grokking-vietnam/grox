package grox.commands

import com.monovore.decline._

case class ParseCommand(verbose: Boolean)

object ParseCommand {

  val verboseOpts = Opts.flag("verbose", help = "Show verbose error").orFalse

  val helpOpts: Opts[ParseCommand] =
    Opts.subcommand("Helper", "DummyHelper") {
      verboseOpts
        .map(ParseCommand.apply)
    }

}
