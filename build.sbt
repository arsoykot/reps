name := "REPS"
version := "1.0.0"
scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  "com.github.tototoshi" %% "scala-csv" % "1.3.10",
  "com.lihaoyi"          %% "ujson"     % "3.1.3",
  "org.scalatest"        %% "scalatest" % "3.2.15" % Test
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint"
)

run / connectInput := true
outputStrategy := Some(StdoutOutput)
