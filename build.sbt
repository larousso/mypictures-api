import sbtprotobuf.{ProtobufPlugin=>PB}

name := """mypictures-api"""

version := "1.0"

maintainer := "Alexandre Delègue"

packageSummary := "Api de gestion de photos"

packageDescription := "Api de gestion de photos"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.mavenLocal, Resolver.sonatypeRepo("releases"), Resolver.jcenterRepo
)

enablePlugins(JavaServerAppPackaging)

val akkaVersion = "2.4.8"


libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-core" % akkaVersion,
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "com.typesafe.akka" %% "akka-http-experimental" % akkaVersion,
  "com.typesafe.akka" %% "akka-persistence" % akkaVersion,
  "com.softwaremill.akka-http-session" %% "core" % "0.2.5",
  "com.softwaremill.akka-http-session" %% "jwt"  % "0.2.5",
  "ch.megard" %% "akka-http-cors" % "0.1.2",
  "com.typesafe.akka" %% "akka-persistence-tck" % akkaVersion,
  "org.iq80.leveldb"            % "leveldb"          % "0.7",
  "org.fusesource.leveldbjni"   % "leveldbjni-all"   % "1.8",
  "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.0",
  "com.sksamuel.scrimage" %% "scrimage-io-extra" % "2.1.0",
  "org.scalaz" %% "scalaz-core" % "7.2.3",
  "org.json4s" %% "json4s-jackson" % "3.3.0",
  "org.json4s" %% "json4s-scalaz" % "3.3.0",
  "de.heikoseeberger" %% "akka-http-json4s" % "1.7.0",
  "com.projectseptember" %% "freek" % "0.4.3",
  "org.typelevel" %% "cats" % "0.6.0",
  "org.typelevel" %% "cats-free" % "0.6.0",
  "org.typelevel" %% "cats-laws" % "0.6.0",
  "com.milessabin" % "si2712fix-library" % "1.2.0" cross CrossVersion.full,
  "com.chuusai" %% "shapeless" % "2.3.1",
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % "test",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.specs2" %% "specs2-core" % "3.8.3" % "test"
)

mainClass in (Compile, run) := Some("com.adelegue.mypictures.Bootstrap")

PB.protobufSettings

javaSource in PB.protobufConfig <<= (sourceDirectory in Compile)(_ / "generated")

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)