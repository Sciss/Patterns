lazy val baseName             = "Patterns"
lazy val baseNameL            = baseName.toLowerCase
lazy val projectVersion       = "0.1.0-SNAPSHOT"

val deps = new {
  val main = new {
    val numbers             = "0.1.3"
    val optional            = "1.0.0"
  }

  val test = new {
    val scalaCollider       = "1.23.0"
    val ugens               = "1.17.1"
    val scalaColliderSwing  = "1.35.0"
    val scalaTest           = "3.0.4"
  }
}

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "Translating SuperCollider's patterns to Scala",
  homepage            := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion        := "2.12.4",
  crossScalaVersions  := Seq("2.12.4", "2.11.12"),
  licenses            := Seq(lgpl2),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
  libraryDependencies ++= Seq(
    "de.sciss"      %% "numbers"                      % deps.main.numbers,
    "de.sciss"      %% "optional"                     % deps.main.optional,
    "de.sciss"      %% "scalacollider"                % deps.test.scalaCollider       % "test",
    "de.sciss"      %% "scalacolliderswing-plotting"  % deps.test.scalaColliderSwing  % "test",
    "de.sciss"      %% "scalacolliderugens-plugins"   % deps.test.ugens               % "test",
    "org.scalatest" %% "scalatest"                    % deps.test.scalaTest           % "test"
  ),
  mainClass in (Test, run) := Some("de.sciss.patterns.RonWithESP")
)

lazy val lgpl2 = "LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")

lazy val root = Project(id = baseNameL, base = file("."))
  .settings(commonSettings)

