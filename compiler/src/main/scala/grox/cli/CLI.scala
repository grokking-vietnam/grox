package grox.cli

import java.nio.file.Path

import cats.implicits.*

import com.monovore.decline.Opts

object CLI:

  enum Command:
    case Scan(file: Path)
    case Parse(file: Path)

  val parse: Opts[Command] =
    val scan =
      Opts.subcommand[Command]("scan", "Scan file to tokens")(
        Opts.argument[Path]("path").map(Command.Scan(_))
      )

    val parse: Opts[Command] =
      Opts.subcommand[Command]("parse", "Parse file to abstract syntax tree")(
        Opts.argument[Path]("path").map(Command.Parse(_))
      )

    scan <+> parse
