name := "fsmmin"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "Twitter" at "http://maven.twttr.com"

packAutoSettings

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "com.twitter.finatra" % "finatra-http_2.11" % "2.1.0"


