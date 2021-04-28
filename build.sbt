name := "functionally-curious"

version := "0.1"

scalaVersion := "2.13.3"

val catsVersion = "2.5.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-free" % catsVersion,
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

scalacOptions ++= Seq(
  "-language:higherKinds"
)