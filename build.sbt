organization := "com.refunctoring"

version in ThisBuild := "0.1-SNAPSHOT"

publishTo in ThisBuild := Some {
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
}
publishArtifact in Test := false
licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))

lazy val foldlScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint:-missing-interpolator,_",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-language:existentials",
  "-language:implicitConversions",
  "-language:higherKinds"
)

pomIncludeRepository := { _ => false }
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

developers := List(
  Developer(
    id="amarpotghan",
    name="Amar Potghan",
    email="amarpotghan@gmail.com",
    url=url("https://refunctoring.com")
  ))

lazy val sonataCredentials = for {
  username <- sys.env.get("SONATYPE_USERNAME")
  password <- sys.env.get("SONATYPE_PASSWORD")
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)

credentials in ThisBuild ++= sonataCredentials.toSeq

crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.6")
compileOrder in ThisBuild := CompileOrder.JavaThenScala

lazy val foldSettings = Seq(
  version := "0.0.1",
  scalaVersion := "2.12.6",
  scalacOptions ++= foldlScalacOptions,
  scalacOptions in Test ++= Seq("-Yrangepos"),
  licenses += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
  libraryDependencies ++=
    Seq("org.specs2" %% "specs2-core" % "4.2.0" % "test",
        "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
        "org.scalaz" %% "scalaz-core" % "7.2.25")
)

val projectName = "fold"

lazy val root = (project in file(".")).
  settings(foldSettings: _*).
  settings(name := projectName)
