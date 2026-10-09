lazy val root = project.in(file(".")).settings(
  name := "homeworks",
  scalaVersion := "3.3.6",
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
  ),
  logLevel := Level.Warn,
  maxErrors := 10, // maximum number of errors shown by the Scala compiler
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12",
  run / watchTriggers += baseDirectory.value.toGlob / "*.tests"
)