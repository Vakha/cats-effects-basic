name := "cats-effect-tutorial"

version := "3.5.7"

scalaVersion := "2.13.13"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.7" withSources() withJavadoc()

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
)