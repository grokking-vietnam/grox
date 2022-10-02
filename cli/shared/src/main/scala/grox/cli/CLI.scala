package grox.cli

import cats.syntax.all.*

import com.monovore.decline.Opts

object CLI:

  enum Command:
    case Scan(file: String)
    case Parse(file: String)
    case Evaluate(file: String)
    case Run(file: String)

  val command =
    val scan = Opts.option[String]("scan", "Scan file to tokens").map(Command.Scan(_))

    val parse = Opts
      .option[String]("parse", "Parse file to abstract syntax tree")
      .map(Command.Parse(_))

    val evaluate = Opts
      .option[String]("evaluate", "Evaluate file to grox object")
      .map(Command.Evaluate(_))

    val run = Opts.
      option[String]("run", "Run grox file")
      .map(Command.Run(_))

    scan <+> parse <+> evaluate <+> run

  val debug = Opts.flag("debug", help = "Print debug logs", short = "d").orFalse

  case class Config(command: Command, debug: Boolean)

  val parse = (command, debug).mapN(Config.apply)
