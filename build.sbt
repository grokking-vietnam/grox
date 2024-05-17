inThisBuild(
  Seq(
    scalaVersion := "3.4.2",
    versionScheme := Some("early-semver"),

    // Github Workflow
    githubWorkflowPublishTargetBranches := Seq(), // Don't publish anywhere
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("21")),
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
      WorkflowStep.Use(
        UseRef.Public("peaceiris", "actions-gh-pages", "v3"),
        params = Map(
          "github_token" -> "${{ secrets.GITHUB_TOKEN }}",
          "publish_dir" -> "./website/build",
          "user_name" -> "github-actions[bot]",
          "user_email" -> "41898282+github-actions[bot]@users.noreply.github.com",
        ),
        name = Some("Deploy to Github Pages"),
        cond = Some("${{ github.ref == 'refs/heads/main' }}"),
      ),
    ),
  )
)

val commonSettings = Seq(
  scalacOptions -= "-Xfatal-warnings",
  scalacOptions += "-source:future",
  scalacOptions += "-rewrite",
  scalacOptions += "-indent",
  scalacOptions += "-Yexplicit-nulls",
  scalacOptions += "-explain",
  scalacOptions += "-Wunused:all",
  libraryDependencies ++= Seq(
    Dependencies.catsCore.value,
    Dependencies.scribe.value,
    Dependencies.scribeCats.value,
    Dependencies.catsEffect.value,
    Dependencies.fs2.value,
    Dependencies.scalaCheckEffect.value,
    Dependencies.munit.value,
    Dependencies.munitCatsEffect.value,
    Dependencies.munitScalaCheck.value,
    Dependencies.munitScalaCheckEffect.value,
  ),
)

val commonJsSettings = Seq(
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
)

val kantan = crossProject(JSPlatform, JVMPlatform)
  .settings(scalacOptions -= "-Xfatal-warnings", scalacOptions += "-Wconf:all")
  .jsSettings(commonJsSettings)

val compiler = crossProject(JSPlatform, JVMPlatform)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      Dependencies.catsParse.value
    ),
  )
  .dependsOn(kantan)
  .jsSettings(commonJsSettings)

val cli = crossProject(JSPlatform, JVMPlatform)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
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
  .aggregate(compiler.js, compiler.jvm, cli.js, cli.jvm, web, kantan.js, kantan.jvm)

// Commands
addCommandAlias("build", "buildJs")
addCommandAlias("buildJs", ";root/fullLinkJS")
addCommandAlias("testAll", "all test")
addCommandAlias("prepare", "fmt")
addCommandAlias("fmt", "all root/scalafmtSbt root/scalafmtAll")
addCommandAlias("fmtCheck", "all root/scalafmtSbtCheck root/scalafmtCheckAll")
addCommandAlias("check", "fmtCheck")
