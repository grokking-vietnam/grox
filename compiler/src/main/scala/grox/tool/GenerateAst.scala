package grox.tool

object GenerateAst {

  // TODO: Should we implement this?
  def main(args: Seq[String]) = defineAst(
    "Expr",
    Seq(
      "Binary   : Expr left, Token operator, Expr right",
      "Grouping : Expr expression",
      "Literal  : Object value",
      "Unary    : Token operator, Expr right",
    ),
  )

  def defineAst(baseName: String, grammar: Seq[String]): Unit = {}
}
