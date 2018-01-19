lazy val baseName             = "Patterns"
lazy val baseNameL            = baseName.toLowerCase
lazy val projectVersion       = "0.1.0-SNAPSHOT"

val deps = new {
  val main = new {
    val numbers             = "0.1.3"
    val optional            = "1.0.0"
    val serial              = "1.0.3"
    val soundProcesses      = "3.16.1"
  }

  val test = new {
    val lucre               = "3.5.0"
    val scalaCollider       = "1.23.0"
    val scalaColliderSwing  = "1.35.0"
    val scalaTest           = "3.0.4"
    val ugens               = "1.17.1"
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
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint")
)

lazy val lgpl2 = "LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")

lazy val root = project.in(file("."))
  .aggregate(core, lucre)
  .dependsOn(core, lucre)
  .settings(commonSettings)
  .settings(
    name := baseName
  )

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(
    name := s"$baseName-core",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "numbers"                      % deps.main.numbers,
      "de.sciss"      %% "optional"                     % deps.main.optional,
      "de.sciss"      %% "serial"                       % deps.main.serial,
      "de.sciss"      %% "scalacollider"                % deps.test.scalaCollider       % "test",
      "de.sciss"      %% "scalacolliderswing-plotting"  % deps.test.scalaColliderSwing  % "test",
      "de.sciss"      %% "scalacolliderugens-plugins"   % deps.test.ugens               % "test",
      "org.scalatest" %% "scalatest"                    % deps.test.scalaTest           % "test"
    ),
    mainClass in (Test, run) := Some("de.sciss.patterns.RonWithESP")
  )

lazy val bdb = "bdb"  // either "bdb" or "bdb6"

lazy val lucre = project.in(file("lucre"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := s"$baseName-lucre",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "soundprocesses-core"  % deps.main.soundProcesses,
      "de.sciss"      %% s"lucre-$bdb"          % deps.test.lucre           % "test",
      "org.scalatest" %% "scalatest"            % deps.test.scalaTest       % "test"
    )
  )