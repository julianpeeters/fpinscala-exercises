scalaVersion := "2.12.2"

version := "0.1.0-SNAPSHOT"

lazy val scalaz = "org.scalaz" %% "scalaz-core" % "7.2.12"

lazy val specs2 = "org.specs2" %% "specs2-core" % "3.8.6" % "test"

lazy val `chapter01` =
  project.in(file("chapter01"))
    .settings(
      libraryDependencies ++= Seq(specs2))

lazy val `chapter02` =
  project.in(file("chapter02"))
    .settings(
      libraryDependencies ++= Seq(specs2))

lazy val `chapter03` =
  project.in(file("chapter03"))
    .settings(
      libraryDependencies ++= Seq(specs2))

lazy val `chapter04` =
  project.in(file("chapter04"))
    .settings(
      libraryDependencies ++= Seq(specs2))