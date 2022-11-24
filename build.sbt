ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "fragsize",
    version := "1.0",
    scalaVersion := "2.13.10",
    mainClass in Compile := Some("Main.BamAnalyze"),
    assembly / assemblyJarName := "fragsize.jar"
  )

libraryDependencies += "com.github.samtools" % "htsjdk" % "3.0.2"
libraryDependencies += "com.lihaoyi" %% "upickle" % "0.9.5"
libraryDependencies += "com.google.code.gson" % "gson" % "2.10"
libraryDependencies += "org.json4s" %% "json4s-core" % "4.1.0-M2"
libraryDependencies += "org.json4s" %% "json4s-native" % "4.1.0-M2"
libraryDependencies += "net.liftweb" %% "lift-json-ext" % "3.5.0"
libraryDependencies += "net.liftweb" %% "lift-json-ext" % "3.5.0"


// META-INF discarding
ThisBuild / assemblyMergeStrategy := {
  _ => MergeStrategy.first
}


