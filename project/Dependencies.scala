import sbt._

object Dependencies {

  object Cats {
    val catsEffect = "org.typelevel" %% "cats-effect" % "3.2.9"
    val catsMtl = "org.typelevel" %% "cats-mtl" % "1.2.1"
    val catsParse = "org.typelevel" %% "cats-parse" % "0.3.4"

    val munitCatsEffect = "org.typelevel" %% "munit-cats-effect-3" % "1.0.6" % Test

    val all = Seq(catsEffect, catsMtl, catsParse, munitCatsEffect)
  }

}
