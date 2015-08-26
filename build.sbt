name := "fpscala"
description := "Working through Manning's Functional Programming in Scala"
organization := "nl.jqno.fpscala"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.7"
scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature", "-unchecked", "-Xlint", "-Xfatal-warnings")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.0" % Test
)

