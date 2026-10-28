import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

enablePlugins(ScalaJSPlugin)

val scala3 = "3.7.4"

val SaxVersion          = "2.0.2.10-SNAPSHOT"
val XercesVersion       = "2.11.0.17-SNAPSHOT"
val ScalaJsStubsVersion = "1.1.0"
val UTestVersion        = "0.8.9"

ThisBuild / scalaVersion      := scala3
ThisBuild / githubOwner       := "orbeon"
ThisBuild / githubRepository  := "msv"
ThisBuild / githubTokenSource := TokenSource.Environment("GITHUB_TOKEN")
ThisBuild / traceLevel        := 0

lazy val msv = (crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full) in file("msv"))
  .settings(
    organization := "org.orbeon",
    name         := "msv",
    version      := "20081113.1-SNAPSHOT",

    scalaVersion := scala3,

    Compile / doc / sources                := Seq.empty,
    Compile / packageDoc / publishArtifact := false,

    scalacOptions ++= Seq(
      "-encoding", "utf8",
      "-deprecation",
      "-unchecked",
      "-feature"
    ),

    Compile / javaSource := baseDirectory.value / "dummy",
    Test    / javaSource := baseDirectory.value / "dummy",

    libraryDependencies += "com.lihaoyi" %%% "utest" % UTestVersion % Test,
    testFrameworks      += new TestFramework("utest.runner.Framework")
  )
  .jvmSettings(
    Compile / unmanagedSources / excludeFilter := "*.java",
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % ScalaJsStubsVersion % Provided,
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      ("org.xml"    %%% "sax"    % SaxVersion).cross(CrossVersion.for2_13Use3),
      ("org.orbeon" %%% "xerces" % XercesVersion).cross(CrossVersion.for2_13Use3)
    )
  )

lazy val msvJS  = msv.js
lazy val msvJVM = msv.jvm

lazy val root = project.in(file("."))
  .aggregate(msvJS, msvJVM)
  .settings(
    publish                       := {},
    publishLocal                  := {},
    ThisProject / sourceDirectory := baseDirectory.value / "root",
    crossScalaVersions            := Nil
  )
