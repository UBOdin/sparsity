import scala.sys.process._

name := "Sparsity"
version := "1.8"
organization := "info.mimirdb"
scalaVersion := "2.12.10"
crossScalaVersions := Seq("2.11.11", "2.12.10 ")

dependencyOverrides += "org.scala-lang" % "scala-library" % scalaVersion.value

resolvers += "MimirDB" at "https://maven.mimirdb.info/"
resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq(
  "com.lihaoyi"                   %% "fastparse"                 % "2.1.0",
  "com.typesafe.scala-logging"    %%  "scala-logging"            % "3.9.2",
  "ch.qos.logback"                %   "logback-classic"          % "1.1.7",
  "org.specs2"                    %%  "specs2-core"              % "4.6.0" % "test",
  "org.specs2"                    %%  "specs2-junit"             % "4.6.0" % "test"
)

////// Publishing Metadata //////
// use `sbt publish makePom` to generate 
// a publishable jar artifact and its POM metadata

publishMavenStyle := true

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
  </scm>

/////// Publishing Options ////////
// use `sbt publish` to update the package in 
// your own local ivy cache
publishTo := Some(Resolver.file("file",  new File("/var/www/maven_repo/")))
