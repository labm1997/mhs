import Dependencies._

parallelExecution in test := false
lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "br.unb.cic.tp1",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
    libraryDependencies += "com.github.nikita-volkov" % "sext" % "0.2.4"
  )
