import sbt.Tests.Setup

lazy val root = (project in file(".")).
  settings(
    name := "Spit",
    version := "1.0",
    scalaVersion := "2.12.1"
  )

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.0"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.0"
libraryDependencies += "com.typesafe.akka" % "akka-slf4j_2.12" % "2.5.1"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"


testOptions += Setup( cl =>
  cl.loadClass("org.slf4j.LoggerFactory").
    getMethod("getLogger",cl.loadClass("java.lang.String")).
    invoke(null,"ROOT")
)

