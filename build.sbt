enablePlugins(ScalaNativePlugin)

scalaVersion := "2.11.8"


// set the main class for 'sbt run'
mainClass in (Compile, run) := Some("example.Main")
