enablePlugins(ScalaJSPlugin)

name := "Metanet Viewer"
scalaVersion := "2.12.2" // or any other Scala version >= 2.10.2

scalaVersion in ThisBuild := "2.12.2"

// This is an application with a main method
scalaJSUseMainModuleInitializer := false

scalacOptions ++= Seq("-deprecation")

resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"
libraryDependencies += "org.singlespaced" %%% "scalajs-d3" % "0.3.4"
libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"

val circeVersion = "0.8.0"

libraryDependencies ++= Seq(
  "io.circe" %%% "circe-core",
  "io.circe" %%% "circe-generic",
  "io.circe" %%%  "circe-parser"
).map(_ % circeVersion)


jsDependencies += "org.webjars" % "d3js" % "3.5.17" / "3.5.17/d3.min.js"
jsDependencies += RuntimeDOM
jsDependencies += ProvidedJS / "tsne.js"