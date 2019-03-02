lazy val baseName           = "Patterns"
lazy val baseNameL          = baseName.toLowerCase
lazy val projectVersion     = "0.8.0-SNAPSHOT"
lazy val mimaVersion        = "0.8.0"

val deps = new {
  val core = new {
    val lucre               = "3.11.0"
    val numbers             = "0.2.0"
    val optional            = "1.0.0"
    val serial              = "1.1.1"
  }
  val lucre = new {
    val soundProcesses      = "3.25.0-SNAPSHOT"
  }
  val test = new {
    val kollFlitz           = "0.2.3"
    val scalaCollider       = "1.28.0"
    val scalaColliderSwing  = "1.41.0"
    val scalaTest           = "3.0.6"
    val ugens               = "1.19.2"
  }
}

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "Translating SuperCollider's patterns to Scala",
  homepage            := Some(url(s"https://git.iem.at/sciss/$baseName")),
  scalaVersion        := "2.13.0-M5",
  crossScalaVersions  := Seq("2.12.8", "2.11.12", "2.13.0-M5"),
  licenses            := Seq(lgpl2),
  scalacOptions      ++= Seq(
    "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint", "-Xsource:2.13"
  ),
  resolvers           += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
  updateOptions       := updateOptions.value.withLatestSnapshots(false)
) ++ publishSettings

lazy val lgpl2 = "LGPL v2.1+" -> url("http://www.gnu.org/licenses/lgpl-2.1.txt")

lazy val root = project.in(file("."))
  .aggregate(core, lucre)
  .dependsOn(core, lucre)
  .settings(commonSettings)
  .settings(
    name := baseName
  )

lazy val publishSettings = Seq(
  // ---- publishing ----
  publishMavenStyle := true,
  publishTo := {
    Some(if (isSnapshot.value)
      "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    else
      "Sonatype Releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2"
    )
  },
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  pomExtra := { val n = baseName
  <scm>
    <url>git@git.iem.at:sciss/{n}.git</url>
    <connection>scm:git:git@git.iem.at:sciss/{n}.git</connection>
  </scm>
    <developers>
      <developer>
        <id>sciss</id>
        <name>Hanns Holger Rutz</name>
        <url>http://www.sciss.de</url>
      </developer>
    </developers>
  }
)

lazy val testSettings = Seq(
  libraryDependencies += {
    "org.scalatest" %% "scalatest" % deps.test.scalaTest % Test
  }
)

lazy val core = project.in(file("core"))
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := s"$baseName-core",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "numbers"                      % deps.core.numbers,
      "de.sciss"      %% "optional"                     % deps.core.optional,
      "de.sciss"      %% "serial"                       % deps.core.serial,
      "de.sciss"      %% "lucre-aux"                    % deps.core.lucre,
      "de.sciss"      %% "lucre-data"                   % deps.core.lucre,
      "de.sciss"      %% "kollflitz"                    % deps.test.kollFlitz           % Test,
      "de.sciss"      %% "scalacollider"                % deps.test.scalaCollider       % Test,
      "de.sciss"      %% "scalacolliderswing-plotting"  % deps.test.scalaColliderSwing  % Test,
      "de.sciss"      %% "scalacolliderugens-core"      % deps.test.ugens               % Test, // sbt fuck up
      "de.sciss"      %% "scalacolliderugens-plugins"   % deps.test.ugens               % Test,
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% baseNameL % mimaVersion),
    mainClass in (Test, run) := Some("de.sciss.patterns.RonWithESP")
  )

lazy val bdb = "bdb"  // either "bdb" or "bdb6"

lazy val lucre = project.in(file("lucre"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := s"$baseName-lucre",
    libraryDependencies ++= Seq(
      "de.sciss"      %% "soundprocesses-core"  % deps.lucre.soundProcesses,
      "de.sciss"      %% s"lucre-$bdb"          % deps.core.lucre  % Test
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-lucre" % mimaVersion)
  )

