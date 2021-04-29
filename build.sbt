name := "SuraPractice"

version := "0.1"

scalaVersion := "2.13.5"


val AkkaVersion = "2.6.8"
val AkkaHttpVersion = "10.2.4"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % AkkaHttpVersion,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.typelevel" %% "cats-core" % "2.2.0",
  "org.slf4j" % "slf4j-log4j12" % "1.2" % Test
)