import sbt._

object Dependencies {

  val declineVersion = "2.2.0"
  val catsEffectVersion =  "3.3.5"
  val catsMtlVersion = "1.2.1"
  val catsParseVersion = "0.3.6"


  object Cats {
    val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion
    val catsMtl = "org.typelevel" %% "cats-mtl" % catsMtlVersion
    val catsParse = "org.typelevel" %% "cats-parse" % catsParseVersion
    val decline = "com.monovore" %% "decline" % declineVersion
    val declineEffect = "com.monovore" %% "decline-effect" % declineVersion

    val all = Seq(catsEffect, catsMtl, catsParse, decline, declineEffect)
  }

  object Tests {
    val munit = "org.scalameta" %% "munit" % "0.7.29" % Test
    val munitCatsEffect = "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
    val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.15.4" % Test
    val munitScalaCheck = "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test

    val all = Seq(munit, munitScalaCheck, munitCatsEffect, scalaCheck)
  }

  val all = Cats.all ++ Tests.all

}
