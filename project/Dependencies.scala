import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  object Cats {

    val declineVersion = "2.2.0"

    val catsCore = Def.setting("org.typelevel" %%% "cats-core" % "2.7.0")
    val catsEffect = Def.setting("org.typelevel" %%% "cats-effect" % "3.3.12")
    val catsParse = Def.setting("org.typelevel" %%% "cats-parse" % "0.3.7")

    val decline = Def.setting("com.monovore" %%% "decline" % declineVersion)
    val declineEffect = Def.setting("com.monovore" %%% "decline-effect" % declineVersion)

    val all = Seq(catsEffect, catsCore, catsParse, declineEffect)
  }

  object Tests {
    val munit = Def.setting("org.scalameta" %% "munit" % "0.7.29" % Test)
    val munitCatsEffect = Def.setting("org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test)
    val scalaCheck = Def.setting("org.scalacheck" %% "scalacheck" % "1.16.0" % Test)
    val munitScalaCheck = Def.setting("org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test)

    val all = Seq(munit, munitScalaCheck, munitCatsEffect, scalaCheck)
  }

  val all: Seq[Object] = Cats.all ++ Tests.all

}
