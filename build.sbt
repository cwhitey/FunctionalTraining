name := "FunctionalTraining"

organization := "rea-group.com"

version := "0.1.0-SNAPSHOT"

//scalaVersion := "2.11.0"
scalaVersion in ThisBuild := "2.12.4"

libraryDependencies ++= Seq("org.scalaz" %% "scalaz-core" % "7.2.19",
                            "org.scalaz" %% "scalaz-effect" % "7.2.19",
                            "org.specs2" %% "specs2-core" % "4.0.2",
                            "org.typelevel" %% "scalaz-specs2" % "0.5.2")

initialCommands := "import com.rea.higherorder._; import com.rea.typesafety._; import Composing._; import ValidationExercises._"
