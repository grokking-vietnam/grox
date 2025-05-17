package grox

import scala.scalajs.js.annotation.*

import cats.effect.IO

import scribe.cats.*
import tyrian.Html.*
import tyrian.*

@JSExportTopLevel("TyrianApp")
object Playground extends TyrianIOApp[Msg, Model]:

  override def router: Location => Msg = Routing.none(Msg.NoOp)

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) = (Model("", ""), Cmd.None)

  def eval(f: Executor[IO] => IO[String]): Cmd[IO, Msg] = Cmd.Run(
    Executor
      .module[IO]
      .use(f)
  )(Msg.Result.apply)

  def scan(source: String): Cmd[IO, Msg] = eval(exec =>
    exec
      .scan(source)
      .map(tokens => tokens.mkString("\n"))
      .handleError(err => s"Error: ${err.toString}")
  )

  def parse(source: String): Cmd[IO, Msg] = eval(exec =>
    exec
      .parse(source)
      .map(_.toString)
      .handleError(err => s"Error: ${err.toString}")
  )

  def run(source: String): Cmd[IO, Msg] = eval(exec =>
    exec
      .execute(source)
      .compile
      .toList
      .map(_.mkString("\n"))
      .handleError(err => s"Error: ${err.toString}")
  )

  def view(model: Model): Html[Msg] = div(
    input(
      placeholder := "Type a grox program",
      onInput(s => Msg.Update(s)),
      myStyle,
    ),
    button(onClick(Msg.Scan))("Scan"),
    button(onClick(Msg.Parse))("Parse"),
    button(onClick(Msg.Run))("Run"),
    p(styles("text-align" -> "center"))(text(model.result)),
  )

  private val myStyle = styles(
    "width" -> "100%",
    "height" -> "40px",
    "padding" -> "10px 0",
    "font-size" -> "2em",
    "text-align" -> "center",
    "margin-bottom" -> "0.25em",
  )

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.Update(str) => (model.copy(input = str, result = ""), Cmd.None)
    case Msg.Result(str) => (model.copy(result = str), Cmd.None)
    case Msg.Scan        => (model, scan(model.input))
    case Msg.Parse       => (model, parse(model.input))
    case Msg.Run         => (model, run(model.input))
    case Msg.NoOp        => (model, Cmd.None)

  def subscriptions(model: Model): Sub[IO, Msg] = Sub.None

case class Model(val input: String, val result: String)

enum Msg:
  case Update(val str: String)
  case Result(val str: String)
  case Scan
  case Parse
  case Run
  case NoOp
