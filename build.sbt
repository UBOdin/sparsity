import scala.sys.process._
import scala.scalanative.build._
import sbtcrossproject.CrossPlugin.autoImport._
import sbtassembly.AssemblyPlugin.autoImport._

val Scala3 = "3.3.6"
val Scala2_13 = "2.13.17"
val Scala2_12 = "2.12.20"

val circeVersion = "0.14.14"
val circeOpticsVersion = "0.15.1"
val jsonPathVersion = "0.2.0"
val specs2Version = "4.23.0"
val fastparseVersion = "3.1.1"
val javaJqVersion = "2.0.0"
val scribeVersion = "3.17.0"
val monocleVersion = "3.3.0"
val mainArgsVersion = "0.7.7"

ThisBuild / scalafmtOnCompile := true
ThisBuild / name := "Sparsity"
ThisBuild / version := "1.7.1-SNAPSHOT"
ThisBuild / organization := "info.mimirdb"
ThisBuild / scalaVersion := Scala2_13
ThisBuild / crossScalaVersions := Seq(Scala2_12, Scala2_13, Scala3)

ThisBuild / dependencyOverrides ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _)) => Seq.empty // Scala 3 doesn't need override
    case _ => Seq("org.scala-lang" % "scala-library" % scalaVersion.value)
  }
}

ThisBuild / resolvers += "MimirDB" at "https://maven.mimirdb.info/"
ThisBuild / resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

val scalacheckVersion = "1.19.0"

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "mainargs" % mainArgsVersion,
    "com.lihaoyi" %%% "fastparse" % fastparseVersion,
    "com.outr" %%% "scribe" % scribeVersion,
    "io.circe" %%% "circe-core" % circeVersion,
    "io.circe" %%% "circe-generic" % circeVersion,
    "io.circe" %%% "circe-generic-extras" % "0.14.5-RC1",
    "io.circe" %%% "circe-parser" % circeVersion,
    "io.circe" %%% "circe-optics" % circeOpticsVersion,
    "dev.optics" %% "monocle-core"  % monocleVersion,
    "dev.optics" %% "monocle-macro" % monocleVersion,
    "com.quincyjo" %%% "scala-json-path-circe" % jsonPathVersion,
    "com.arakelian" % "java-jq" % javaJqVersion,
    "org.specs2" %%% "specs2-core" % specs2Version % "test",
    "org.specs2" %%% "specs2-junit" % specs2Version % "test",
    "org.scalacheck" %%% "scalacheck" % scalacheckVersion % "test"
  )
)

val nativeSettings = Seq(
  nativeConfig ~= {
    _.withLTO(LTO.full)
      .withMode(Mode.releaseFast)
      .withGC(GC.commix)
  }
)

// lazy val root = project
//   .in(file("."))
//   .aggregate(sparsity.jvm, sparsity.native)

lazy val sparsity =
  //crossProject(JVMPlatform, NativePlatform)
  //.crossType(CrossType.Pure)
  project
  .in(file("."))
  .settings(
    name := "Sparsity",
    version := "1.7.1-SNAPSHOT",
    organization := "info.mimirdb",
    commonSettings,
    // Assembly settings
    assembly / mainClass := Some("sparsity.Main"),
    assembly / assemblyJarName := s"${name.value}-${version.value}.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case x => MergeStrategy.first
    },
    assembly / artifact := {
      val art = (assembly / artifact).value
      art.withClassifier(Some("assembly"))
    },
    addArtifact(assembly / artifact, assembly),
    // Publishing settings
    publishMavenStyle := true,
    pomExtra := <url>http://github.com/UBOdin/sparsity/</url>
      <licenses>
        <license>
          <name>Apache License 2.0</name>
          <url>http://www.apache.org/licenses/</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:ubodin/sparsity.git</url>
        <connection>scm:git:git@github.com:ubodin/sparsity.git</connection>
      </scm>,
  )
  //.nativeSettings(nativeSettings)
  .enablePlugins(AssemblyPlugin)
