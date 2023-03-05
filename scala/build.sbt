scalaVersion := "3.2.2"

name := "polymer"

version := "0.0.1"

organization := "org.polyeme"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"
libraryDependencies += "org.typelevel" %% "shapeless3-deriving" % "3.0.3"

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"
