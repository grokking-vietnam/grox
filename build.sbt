inThisBuild(
  Seq(
    scalaVersion := "3.1.2",
    versionScheme := Some("early-semver"),

    // Github Workflow
    githubWorkflowPublishTargetBranches := Seq(), // Don't publish anywhere
    githubWorkflowBuild ++= Seq(
      WorkflowStep.Sbt(List("check"), name = Some("Check Formatting")),
      WorkflowStep.Sbt(List("docs/mdoc"), name = Some("Check docs formatting")),
    ),

    // Scalafix
    scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
  )
)

val commonSettings = Seq(
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions += "-source:future",
  scalacOptions += "-rewrite",
  scalacOptions += "-indent",
  libraryDependencies ++= Seq(
    Dependencies.catsCore.value,
    Dependencies.catsEffect.value,
    Dependencies.catsParse.value,
    Dependencies.fs2.value,
    Dependencies.fs2IO.value,
    Dependencies.decline.value,
    Dependencies.declineEffect.value,
    Dependencies.munit.value,
    Dependencies.munitCatsEffect.value,
    Dependencies.munitScalaCheck.value,
  ),
)

val compiler = crossProject(JSPlatform, JVMPlatform)
  .settings(commonSettings)
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )

lazy val root = project
  .in(file("."))
  .settings(publish := {}, publish / skip := true)
  .aggregate(compiler.js, compiler.jvm)

lazy val docs = project // new documentation project
  .in(file("grox-docs")) // important: it must not be docs/
  .dependsOn(root)
  .settings(
    moduleName := "grox-docs",
    mdocVariables := Map("VERSION" -> version.value),
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
