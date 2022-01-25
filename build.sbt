inThisBuild(
  Seq(
    scalaVersion := "3.1.0",
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
  libraryDependencies ++= Dependencies.all,
)

val compiler = project.settings(commonSettings)

lazy val root = project
  .in(file("."))
  .settings(publish := {}, publish / skip := true)
  .aggregate(compiler)

lazy val docs = project // new documentation project
  .in(file("grox-docs")) // important: it must not be docs/
  .dependsOn(root)
  .settings(
    mdocVariables := Map("VERSION" -> version.value)
  )
  .enablePlugins(MdocPlugin, DocusaurusPlugin)

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
