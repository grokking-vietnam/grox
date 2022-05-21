import sbt._

object Dependencies {

  object Cats {

    val declineVersion = "2.2.0"

    val catsEffect = "org.typelevel" %% "cats-effect" % "3.3.12"
    val catsMtl = "org.typelevel" %% "cats-mtl" % "1.2.1"
    val catsParse = "org.typelevel" %% "cats-parse" % "0.3.7"

    val decline = "com.monovore" %% "decline" % declineVersion
    val declineEffect = "com.monovore" %% "decline-effect" % declineVersion

    val all = Seq(catsEffect, catsMtl, catsParse, declineEffect)
  }

  object Tests {
    val munit = "org.scalameta" %% "munit" % "0.7.29" % Test
    val munitCatsEffect = "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
    val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.16.0" % Test
    val munitScalaCheck = "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test

    val all = Seq(munit, munitScalaCheck, munitCatsEffect, scalaCheck)
  }

  val all = Cats.all ++ Tests.all

}
