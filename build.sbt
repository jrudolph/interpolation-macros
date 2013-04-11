resolvers += "spray repo" at "http://repo.spray.io/"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.1"

scalaVersion := "2.10.1"

// crashes because of SI-6743
//scalacOptions += "-Yrangepos"
