import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {

  val declineVersion = "2.5.0"
  val fs2Version = "3.12.2"
  val scribeVersion = "3.17.0"

  val catsCore = Def.setting("org.typelevel" %%% "cats-core" % "2.13.0")
  val catsEffect = Def.setting("org.typelevel" %%% "cats-effect" % "3.6.3")
  val catsParse = Def.setting("org.typelevel" %%% "cats-parse" % "1.1.0")

  val fs2 = Def.setting("co.fs2" %%% "fs2-core" % fs2Version)
  val fs2IO = Def.setting("co.fs2" %%% "fs2-io" % fs2Version)

  val decline = Def.setting("com.monovore" %%% "decline" % declineVersion)
  val declineEffect = Def.setting("com.monovore" %%% "decline-effect" % declineVersion)

  val scribe = Def.setting("com.outr" %%% "scribe" % scribeVersion)
  val scribeCats = Def.setting("com.outr" %%% "scribe-cats" % scribeVersion)

  val tyrian = Def.setting("io.indigoengine" %%% "tyrian-io" % "0.14.0")

  val munit = Def.setting("org.scalameta" %%% "munit" % "1.1.2" % Test)
  val munitCatsEffect = Def.setting("org.typelevel" %%% "munit-cats-effect" % "2.1.0" % Test)
  val scalaCheck = Def.setting("org.scalacheck" %%% "scalacheck" % "1.16.0" % Test)
  val munitScalaCheck = Def.setting("org.scalameta" %%% "munit-scalacheck" % "1.2.0" % Test)
  val scalaCheckEffect = Def.setting("org.typelevel" %%% "scalacheck-effect" % "2.0.0-M2" % Test)

  val munitScalaCheckEffect = Def.setting(
    "org.typelevel" %%% "scalacheck-effect-munit" % "2.0.0-M2" % Test
  )

}
