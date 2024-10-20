import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  val declineVersion = "2.4.1"
  val fs2Version = "3.11.0"
  val scribeVersion = "3.15.1"

  val catsCore = Def.setting("org.typelevel" %%% "cats-core" % "2.12.0")
  val catsEffect = Def.setting("org.typelevel" %%% "cats-effect" % "3.5.4")
  val catsParse = Def.setting("org.typelevel" %%% "cats-parse" % "1.0.0")

  val fs2 = Def.setting("co.fs2" %%% "fs2-core" % fs2Version)
  val fs2IO = Def.setting("co.fs2" %%% "fs2-io" % fs2Version)

  val decline = Def.setting("com.monovore" %%% "decline" % declineVersion)
  val declineEffect = Def.setting("com.monovore" %%% "decline-effect" % declineVersion)

  val scribe = Def.setting("com.outr" %%% "scribe" % scribeVersion)
  val scribeCats = Def.setting("com.outr" %%% "scribe-cats" % scribeVersion)

  val tyrian = Def.setting("io.indigoengine" %%% "tyrian-io" % "0.11.0")

  val munit = Def.setting("org.scalameta" %%% "munit" % "1.0.2" % Test)
  val munitCatsEffect = Def.setting("org.typelevel" %%% "munit-cats-effect" % "2.0.0" % Test)
  val scalaCheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.16.0" % Test)
  val munitScalaCheck = Def.setting("org.scalameta" %%% "munit-scalacheck" % "1.0.0" % Test)
  val scalaCheckEffect = Def.setting("org.typelevel" %%% "scalacheck-effect" % "2.0.0-M2" % Test)

  val munitScalaCheckEffect = Def.setting(
    "org.typelevel" %%% "scalacheck-effect-munit" % "2.0.0-M2" % Test
  )

}
