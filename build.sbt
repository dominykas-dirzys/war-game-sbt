name := "war-game-sbt"

version := "0.1"

scalaVersion := "2.13.5"

val enumeratumVersion = "1.6.1"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion
)