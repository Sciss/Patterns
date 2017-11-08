lazy val baseName           = "Patterns"
lazy val baseNameL          = baseName.toLowerCase
lazy val projectVersion     = "0.1.0-SNAPSHOT"

lazy val numbersVersion     = "0.1.3"
lazy val scalaTestVersion   = "3.0.4"

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "Translating SuperCollider's patterns to Scala",
  homepage            := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion        := "2.12.4",
  crossScalaVersions  := Seq("2.12.4", "2.11.11"),
  licenses            := Seq(lgpl2),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
  libraryDependencies ++= Seq(
    "de.sciss"      %% "numbers"    % numbersVersion,
    "org.scalatest" %% "scalatest"  % scalaTestVersion % "test"
  )
)

lazy val lgpl2 = "LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")

lazy val root = Project(id = baseNameL, base = file("."))
  .settings(commonSettings)

