lazy val baseName           = "Patterns"
lazy val baseNameL          = baseName.toLowerCase
lazy val projectVersion     = "1.0.0"
lazy val mimaVersion        = "1.0.0"

val deps = new {
  val core = new {
    val lucre               = "4.0.0"
    val numbers             = "0.2.1"
    val optional            = "1.0.1"
    val serial              = "2.0.0"
  }
  val lucre = new {
    val soundProcesses      = "4.0.0"
  }
  val test = new {
    val kollFlitz           = "0.2.4"
    val scalaCollider       = "2.0.0"
    val scalaColliderSwing  = "2.0.0"
    val scalaTest           = "3.2.2"
    val ugens               = "1.19.8"
  }
}

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "Translating SuperCollider's patterns to Scala",
  homepage            := Some(url(s"https://git.iem.at/sciss/$baseName")),
  scalaVersion        := "2.13.3",
  crossScalaVersions  := Seq("2.13.3", "2.12.12"),
  licenses            := Seq(agpl),
  scalacOptions      ++= Seq(
    "-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xlint", "-Xsource:2.13"
  ),
  scalacOptions in (Compile, compile) ++= (if (scala.util.Properties.isJavaAtLeast("9")) Seq("-release", "8") else Nil), // JDK >8 breaks API; skip scala-doc
  // resolvers           += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
  updateOptions       := updateOptions.value.withLatestSnapshots(false),
  parallelExecution in Test := false,
) ++ publishSettings

lazy val agpl = "AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")

lazy val root = project.in(file("."))
  .aggregate(core, lucre, macros)
//  .dependsOn(core, lucre)
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
      "de.sciss"      %% "lucre-adjunct"                % deps.core.lucre,
      "de.sciss"      %% "lucre-data"                   % deps.core.lucre,
      "de.sciss"      %% "lucre-core"                   % deps.core.lucre,
      "de.sciss"      %% "kollflitz"                    % deps.test.kollFlitz           % Test,
      "de.sciss"      %% "scalacollider"                % deps.test.scalaCollider       % Test,
      "de.sciss"      %% "scalacolliderswing-plotting"  % deps.test.scalaColliderSwing  % Test,
      "de.sciss"      %% "scalacolliderugens-core"      % deps.test.ugens               % Test, // sbt problem
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
      "de.sciss"        %% "soundprocesses-core"     % deps.lucre.soundProcesses,
      "org.scala-lang"  %  "scala-reflect"           % scalaVersion.value,
      "de.sciss"        %% "scalacolliderugens-core" % deps.test.ugens,             // sbt problem
      "de.sciss"        %% s"lucre-$bdb"             % deps.core.lucre  % Test
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-lucre" % mimaVersion)
  )

lazy val macros = project.in(file("macros"))
  .dependsOn(lucre)
  .settings(commonSettings)
  .settings(
    name := s"$baseName-macros",
    description := s"Macro support for $baseName",
    scalacOptions += "-Yrangepos",  // this is needed to extract source code
    libraryDependencies ++= Seq(
      "de.sciss" %% "soundprocesses-compiler" % deps.lucre.soundProcesses
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-macros" % mimaVersion)
  )
