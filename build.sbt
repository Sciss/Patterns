lazy val baseName           = "Patterns"
lazy val baseNameL          = baseName.toLowerCase
lazy val projectVersion     = "1.4.0-SNAPSHOT"
lazy val mimaVersion        = "1.4.0"

val deps = new {
  val core = new {
    val log                 = "0.1.1"
    val lucre               = "4.4.1"
    val numbers             = "0.2.1"
    val optional            = "1.0.1"
    val serial              = "2.0.1"
  }
  val lucre = new {
    val soundProcesses      = "4.7.0"
  }
  val test = new {
    val kollFlitz           = "0.2.4"
    val scalaCollider       = "2.6.1"
    val scalaColliderSwing  = "2.6.1"
    val scalaTest           = "3.2.3"
    val ugens               = "1.21.1"
  }
}

lazy val commonJvmSettings = Seq(
  crossScalaVersions  := Seq("3.0.0-M3", "2.13.4", "2.12.13"),
)

// sonatype plugin requires that these are in global
ThisBuild / version      := projectVersion
ThisBuild / organization := "de.sciss"

lazy val commonSettings = Seq(
//  version             := projectVersion,
//  organization        := "de.sciss",
  description         := "Translating SuperCollider's patterns to Scala",
  homepage            := Some(url(s"https://git.iem.at/sciss/$baseName")),
  scalaVersion        := "2.13.4",
  licenses            := Seq(agpl),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8"),
  scalacOptions ++= {
    if (isDotty.value) Nil else Seq("-Xlint", "-Xsource:2.13")
  },
  scalacOptions in (Compile, compile) ++= {
    if (!isDotty.value && scala.util.Properties.isJavaAtLeast("9")) Seq("-release", "8") else Nil // JDK >8 breaks API; skip scala-doc
  },
  sources in (Compile, doc) := {
    if (isDotty.value) Nil else (sources in (Compile, doc)).value // dottydoc is pretty much broken
  },
  updateOptions       := updateOptions.value.withLatestSnapshots(false),
  parallelExecution in Test := false,
  concurrentRestrictions in Global ++= Seq(
    Tags.limitAll(2), Tags.limit(Tags.Test, 1) // with cross-builds we otherwise get OutOfMemoryError
  ),
) ++ publishSettings

lazy val agpl = "AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")

lazy val root = project.in(file("."))
  .aggregate(core.js, core.jvm, lucre.js, lucre.jvm, macros)
//  .dependsOn(core, lucre)
  .settings(commonSettings)
  .settings(
    name := baseName
  )

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  developers := List(
    Developer(
      id    = "sciss",
      name  = "Hanns Holger Rutz",
      email = "contact@sciss.de",
      url   = url("https://www.sciss.de")
    )
  ),
  scmInfo := {
    val h = "git.iem.at"
    val a = s"sciss/$baseName"
    Some(ScmInfo(url(s"https://$h/$a"), s"scm:git@$h:$a.git"))
  },
)

lazy val testSettings = Seq(
  libraryDependencies += {
    "org.scalatest" %%% "scalatest" % deps.test.scalaTest % Test
  }
)

lazy val core = crossProject(JVMPlatform, JSPlatform).in(file("core"))
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)
  .settings(testSettings)
  .settings(
    name := s"$baseName-core",
    libraryDependencies ++= Seq(
      "de.sciss" %%% "log"                          % deps.core.log,
      "de.sciss" %%% "numbers"                      % deps.core.numbers,
      "de.sciss" %%% "optional"                     % deps.core.optional,
      "de.sciss" %%% "serial"                       % deps.core.serial,
      "de.sciss" %%% "lucre-adjunct"                % deps.core.lucre,
      "de.sciss" %%% "lucre-data"                   % deps.core.lucre,
      "de.sciss" %%% "lucre-core"                   % deps.core.lucre,
      "de.sciss" %%% "kollflitz"                    % deps.test.kollFlitz           % Test,
      "de.sciss" %%% "scalacollider"                % deps.test.scalaCollider       % Test,
      "de.sciss" %%% "scalacolliderugens-plugins"   % deps.test.ugens               % Test,
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% baseNameL % mimaVersion),
    mainClass in (Test, run) := Some("de.sciss.patterns.RonWithESP")
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "de.sciss" %% "scalacolliderswing-plotting"  % deps.test.scalaColliderSwing  % Test,
    )
  )

lazy val bdb = "bdb"  // either "bdb" or "bdb6"

lazy val lucre = crossProject(JVMPlatform, JSPlatform).in(file("lucre"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .jvmSettings(commonJvmSettings)
  .settings(testSettings)
  .settings(
    name := s"$baseName-lucre",
    libraryDependencies ++= Seq(
      "de.sciss"        %%% "soundprocesses-core"     % deps.lucre.soundProcesses,
//      "org.scala-lang"  %   "scala-reflect"           % scalaVersion.value,
    ),
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-lucre" % mimaVersion)
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "de.sciss"        %%  s"lucre-$bdb"             % deps.core.lucre  % Test
    )
  )

lazy val macros = project.in(file("macros"))
  .dependsOn(lucre.jvm)
  .settings(commonSettings)
  .settings(commonJvmSettings)
  .settings(
    name := s"$baseName-macros",
    description := s"Macro support for $baseName",
    scalacOptions += "-Yrangepos",  // this is needed to extract source code
    libraryDependencies ++= Seq(
      "de.sciss" %% "soundprocesses-compiler" % deps.lucre.soundProcesses
    ),
    unmanagedSourceDirectories in Compile ++= {
      val sourceDirPl = (sourceDirectory in Compile).value
      val sv = CrossVersion.partialVersion(scalaVersion.value)
      val (sub1, sub2) = sv match {
        case Some((2, n)) if n >= 13  => ("scala-2.13+", "scala-2.14-")
        case Some((3, _))             => ("scala-2.13+", "scala-2.14+")
        case _                        => ("scala-2.13-", "scala-2.14-")
      }
      Seq(sourceDirPl / sub1, sourceDirPl / sub2)
    },
    unmanagedSourceDirectories in Test ++= {
      val sourceDirPl = (sourceDirectory in Test).value
      val sv = CrossVersion.partialVersion(scalaVersion.value)
      val (sub1, sub2) = sv match {
        case Some((2, n)) if n >= 13  => ("scala-2.13+", "scala-2.14-")
        case Some((3, _))             => ("scala-2.13+", "scala-2.14+")
        case _                        => ("scala-2.13-", "scala-2.14-")
      }
      Seq(sourceDirPl / sub1, sourceDirPl / sub2)
    },
    mimaPreviousArtifacts := Set("de.sciss" %% s"$baseNameL-macros" % mimaVersion)
  )
