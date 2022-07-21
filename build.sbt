inThisBuild(
  Seq(
    scalaVersion := "3.1.3",
    versionScheme := Some("early-semver"),

    // Github Workflow
    githubWorkflowPublishTargetBranches := Seq(), // Don't publish anywhere
    githubWorkflowUseSbtThinClient := true,
    githubWorkflowEnv := Map("SBT_OPTS" -> "-Xmx2048M"),
    githubWorkflowBuild ++= Seq(
      WorkflowStep.Sbt(List("build"), name = Some("Build projects")),
      WorkflowStep.Sbt(List("check"), name = Some("Check Formatting")),
      WorkflowStep.Sbt(List("docs/mdoc"), name = Some("Check docs formatting")),
      WorkflowStep.Use(
        UseRef.Public("actions", "setup-node", "v2"),
        params = Map(
          "node-version" -> "16.x",
          "cache" -> "yarn",
          "cache-dependency-path" -> "website/yarn.lock",
        ),
        name = Some("Setup Node"),
      ),
      WorkflowStep.Run(
        commands = List(
          "cd ./website",
          "yarn install --frozen-lockfile",
          "yarn build",
        ),
        name = Some("Check build website"),
      ),
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
  scalacOptions += "-Yexplicit-nulls",
  libraryDependencies ++= Seq(
    Dependencies.catsCore.value,
    Dependencies.munit.value,
    Dependencies.munitCatsEffect.value,
    Dependencies.munitScalaCheck.value,
  ),
)

val commonJsSettings = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
)

val compiler = crossProject(JSPlatform, JVMPlatform)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      Dependencies.catsParse.value
    ),
  )
  .jsSettings(commonJsSettings)

val cli = crossProject(JSPlatform, JVMPlatform)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      Dependencies.catsEffect.value,
      Dependencies.fs2.value,
      Dependencies.fs2IO.value,
      Dependencies.decline.value,
      Dependencies.declineEffect.value,
    ),
  )
  .dependsOn(compiler)
  .jsSettings(commonJsSettings, scalaJSUseMainModuleInitializer := true)

val web = project
  .enablePlugins(ScalaJSPlugin)
  .settings(
    commonSettings,
    commonJsSettings,
    libraryDependencies ++= Seq(
      Dependencies.catsEffect.value,
      Dependencies.fs2.value,
      Dependencies.fs2IO.value,
      Dependencies.tyrian.value,
    ),
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := file("./web/target/scala-3"),
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := file("./web/target/scala-3"),
  )
  .dependsOn(compiler.js)

lazy val docs = project // new documentation project
  .in(file("grox-docs")) // important: it must not be docs/
  .dependsOn(root)
  .settings(
    moduleName := "grox-docs",
    mdocVariables := Map("VERSION" -> version.value),
  )
  .enablePlugins(MdocPlugin, DocusaurusPlugin)

lazy val root = project
  .in(file("."))
  .settings(publish := {}, publish / skip := true)
  .aggregate(compiler.js, compiler.jvm, cli.js, cli.jvm, web)

// Commands
addCommandAlias("build", "buildJs")
addCommandAlias("buildJs", ";root/fullLinkJS")
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
