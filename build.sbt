name := "candy"

scalaVersion := "2.12.0"

organization := "org.hablapps"

val monocleVersion = "1.3.2"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.7",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-state" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-refined" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-unsafe" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-law" % monocleVersion % "test"
)

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)
