import Dependencies._

inThisBuild(
  Seq(
    scalaVersion := "3.1.0-RC2",
    versionScheme := Some("early-semver"),

    // Github Workflow
    githubWorkflowPublishTargetBranches := Seq(), // Don't publish anywhere
    githubWorkflowBuild ++= Seq(
      WorkflowStep.Sbt(List("check"), name = Some("Check Formatting"))
    ),

    // Scalafix
    scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
  )
)

val commonSettings = Seq(
  scalacOptions -= "-Xfatal-warnings",
  libraryDependencies ++= Cats.all,
)

val compiler = project.settings(commonSettings)

lazy val root = project
  .in(file("."))
  .settings(publish := {}, publish / skip := true)
  .aggregate(compiler)

// Commands
addCommandAlias("build", "prepare; test")
addCommandAlias("testAll", "all test")
addCommandAlias("prepare", "fix; fmt")
addCommandAlias("fix", "all compile:scalafix test:scalafix")
addCommandAlias(
  "fixCheck",
  "; compile:scalafix --check ; test:scalafix --check",
)
addCommandAlias("fmt", "all root/scalafmtSbt root/scalafmtAll")
addCommandAlias("fmtCheck", "all root/scalafmtSbtCheck root/scalafmtCheckAll")
addCommandAlias("check", "fixCheck; fmtCheck")
