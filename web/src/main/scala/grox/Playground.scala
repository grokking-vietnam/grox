package grox

import scala.scalajs.js.annotation.*

import cats.Functor
import cats.effect.IO
import cats.syntax.all.*

import tyrian.Html.*
import tyrian.*

@JSExportTopLevel("TyrianApp")
object Playground extends TyrianApp[Msg, Model]:

  val exec = Executor.module[Either[Throwable, *]]

  def init(flags: Map[String, String]): (Model, Cmd[IO, Msg]) = (Model("", ""), Cmd.None)

  def update(model: Model): Msg => (Model, Cmd[IO, Msg]) =
    case Msg.Update(str) => (model.copy(input = str, result = ""), Cmd.None)
    case Msg.Scan =>
      val result = exec
        .scan(model.input)
        .fold(t => s"Error: ${t.toString}", tokens => tokens.mkString("\n"))
      (model.copy(result = result), Cmd.None)
    case Msg.Parse =>
      val result = exec.parse(model.input).fold(t => s"Error: ${t.toString}", _.show)
      (model.copy(result = result), Cmd.None)

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
  case Scan
  case Parse
