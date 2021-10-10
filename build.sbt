ThisBuild / scalaVersion := "3.1.0-RC3"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / githubWorkflowPublishTargetBranches := Seq() // Don't publish anywhere
ThisBuild / githubWorkflowBuild ++= Seq(WorkflowStep.Sbt(List("scalafmtCheckAll"), name = Some("Check Formatting")))

val commonSettings = Seq(
  scalacOptions -= "-Xfatal-warnings",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.2.9",
    "org.typelevel" %% "cats-mtl" % "1.2.1",
    "org.typelevel" %% "munit-cats-effect-3" % "1.0.6" % Test,
  ),
)

val compiler = project.settings(commonSettings)

lazy val root = project
  .in(file("."))
  .settings(publish := {}, publish / skip := true)
  .aggregate(compiler)
