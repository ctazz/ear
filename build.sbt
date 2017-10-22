name := "ear"

organization := "org.example"

version := "1.0.-SNAPSHOT"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "org.scalatest"               %%  "scalatest"                         % "3.0.1" % "test"
)


resolvers ++= Seq(
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
  "Scala-tools" at "https://oss.sonatype.org/content/repositories/snapshots"
)

publishMavenStyle := true

parallelExecution in Test := false



