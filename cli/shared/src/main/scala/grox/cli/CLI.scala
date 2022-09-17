package grox.cli

import cats.implicits.*

import com.monovore.decline.Opts

object CLI:

  enum Command:
    case Scan(file: String)
    case Parse(file: String)
    case Evaluate(file: String)
    case Run(file: String)

  val parse: Opts[Command] =
    val scan =
      Opts.subcommand[Command]("scan", "Scan file to tokens")(
        Opts.argument[String]("path").map(Command.Scan(_))
      )

    val parse: Opts[Command] =
      Opts.subcommand[Command]("parse", "Parse file to abstract syntax tree")(
        Opts.argument[String]("path").map(Command.Parse(_))
      )

    val evaluate: Opts[Command] =
      Opts.subcommand[Command]("evaluate", "Evaluate file to grox object")(
        Opts.argument[String]("path").map(Command.Evaluate(_))
      )

    val run: Opts[Command] =
      Opts.subcommand[Command]("run", "Run grox file")(
        Opts.argument[String]("path").map(Command.Run(_))
      )

    scan <+> parse <+> evaluate <+> run
