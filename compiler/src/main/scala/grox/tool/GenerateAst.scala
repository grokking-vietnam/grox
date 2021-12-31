package grox.tool

object GenerateAst {
  def main(args: Seq[String]) =
      defineAst("Expr", Seq(
      "Binary   : Expr left, Token operator, Expr right",
      "Grouping : Expr expression",
      "Literal  : Object value",
      "Unary    : Token operator, Expr right"
    ))
  
    def defineAst(baseName: String, grammar: Seq[String]): Unit = {}
}
