name := """EmpireBuilder"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "com.typesafe.play" %% "play-slick" % "0.8.0",
  "org.webjars" % "bootstrap" % "3.3.1",
  "com.adrianhurt" %% "play-bootstrap3" % "0.1.1"
)
