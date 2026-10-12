lazy val root = project.in(file(".")).settings(
  name := "homeworks",
  scalaVersion := "3.3.6",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
  ),
  logLevel := Level.Info,
  maxErrors := 10,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test,
  run / watchTriggers += baseDirectory.value.toGlob / "*.tests"
)
