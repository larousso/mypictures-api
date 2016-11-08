import sbtprotobuf.{ProtobufPlugin=>PB}

name := """mypictures-api"""

organization := "com.adelegue"

version := "1.0"

maintainer := "Alexandre Delègue"

packageSummary := "Api de gestion de photos"

packageDescription := "Api de gestion de photos"

scalaVersion := "2.11.8"

lazy val root = (project in file(".")).enablePlugins(PlayScala)


resolvers ++= Seq(
  Resolver.mavenLocal, Resolver.sonatypeRepo("releases"), Resolver.jcenterRepo
)

//enablePlugins(JavaServerAppPackaging, UniversalDeployPlugin)

val akkaVersion = "2.4.8"


libraryDependencies ++= Seq(
  filters,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-core" % akkaVersion,
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-experimental" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence" % akkaVersion,
  "com.softwaremill.macwire" %% "macros" % "2.2.4" % "provided",
  "com.softwaremill.macwire" %% "util" % "2.2.4",
  "com.softwaremill.macwire" %% "proxy" % "2.2.4",
  "com.typesafe.akka" %% "akka-persistence-tck" % akkaVersion,
  "org.iq80.leveldb"            % "leveldb"          % "0.7",
  "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8",
  "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.0",
  "com.sksamuel.scrimage" %% "scrimage-io-extra" % "2.1.0",
  "org.typelevel" %% "cats" % "0.8.0",
  //"com.milessabin" % "si2712fix-library" % "1.2.0" cross CrossVersion.full,
  "com.chuusai" %% "shapeless" % "2.3.1",
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test,
  "org.scalatest" %% "scalatest" % "2.2.4" % Test,
  "org.specs2" %% "specs2-core" % "3.8.3" % Test
)

//mainClass in (Compile, run) := Some("com.adelegue.mypictures.Bootstrap")

PB.protobufSettings

//javaSource in PB.protobufConfig <<= (sourceDirectory in Compile)(_ / "generated")

scalacOptions in Test ++= Seq("-Yrangepos", "-Xlog-implicits")

scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
//addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)

publishTo := Some(Resolver.file("file",  new File("/Users/adelegue/idea/mvn-repo/releases")))

