package grox

import scala.scalajs.js.annotation.*

import cats.Functor
import cats.effect.IO
import cats.syntax.all.*

import tyrian.Html.*
import tyrian.*

@JSExportTopLevel("TyrianApp")
object Playground extends TyrianApp[Msg, Model]:

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) = (Model("", ""), Cmd.None)

  def eval(source: String)(f: Executor[IO] => IO[String]): Cmd[IO, Msg] =
    Cmd.Run(
      Executor
        .module[IO]
        .use(f)
    )(Msg.Result.apply)

  def scan(source: String): Cmd[IO, Msg] =
    eval(source)(exec =>
      exec
        .scan(source)
        .map(tokens => tokens.mkString("\n"))
        .handleError(err => s"Error: ${err.toString}")
    )

  def parse(source: String): Cmd[IO, Msg] =
    eval(source)(exec =>
      exec
        .parse(source)
        .map(_.toString)
        .handleError(err => s"Error: ${err.toString}")
    )

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.Update(str) => (model.copy(input = str, result = ""), Cmd.None)
    case Msg.Result(str) => (model.copy(result = str), Cmd.None)
    case Msg.Scan        => (model, scan(model.input))
    case Msg.Parse       => (model, parse(model.input))

  def view(model: Model): Html[Msg] = div(
    input(
      placeholder := "Type a grox program",
      onInput(s => Msg.Update(s)),
      myStyle,
    ),
    button(onClick(Msg.Scan))("Scan"),
    button(onClick(Msg.Parse))("Parse"),
    p(styles("text-align" -> "center"))(text(model.result)),
  )

  def subscriptions(model: Model): Sub[IO, Msg] = Sub.None

  private val myStyle = styles(
    "width" -> "100%",
    "height" -> "40px",
    "padding" -> "10px 0",
    "font-size" -> "2em",
    "text-align" -> "center",
    "margin-bottom" -> "0.25em",
  )

case class Model(val input: String, val result: String)

enum Msg:
  case Update(val str: String)
  case Result(val str: String)
  case Scan
  case Parse
