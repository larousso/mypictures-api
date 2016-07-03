import sbtprotobuf.{ProtobufPlugin=>PB}

name := """mypictures-api"""

version := "1.0"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.mavenLocal, Resolver.sonatypeRepo("releases"), Resolver.jcenterRepo
)

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.6",
  "com.typesafe.akka" %% "akka-stream" % "2.4.6",
  "com.typesafe.akka" %% "akka-http-core" % "2.4.6",
  "com.typesafe.akka" %% "akka-http-experimental" % "2.4.6",
  "com.typesafe.akka" %% "akka-persistence" % "2.4.6",
  "com.softwaremill.akka-http-session" %% "core" % "0.2.5",
  "com.softwaremill.akka-http-session" %% "jwt"  % "0.2.5",
  "com.typesafe.akka" %% "akka-persistence-tck" % "2.4.6",
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
  "com.typesafe.akka" %% "akka-testkit" % "2.4.6" % "test",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.specs2" %% "specs2-core" % "3.8.3" % "test"
)

PB.protobufSettings

javaSource in PB.protobufConfig <<= (sourceDirectory in Compile)(_ / "generated")

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq(
  "-feature",
  "-language:higherKinds"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")
addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)