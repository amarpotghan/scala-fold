lazy val foldlScalacOptions = Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint:-missing-interpolator,_",
  "-Yinline-warnings",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:implicitConversions",
  "-language:higherKinds"
)

lazy val foldSettings = Seq(
  organization := "",
  version := "0.0.1",
  scalaVersion := "2.11.8",
  scalacOptions ++= foldlScalacOptions,
  scalacOptions in Test ++= Seq("-Yrangepos"),
  licenses += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
  libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.4" % "test",
                              "org.scalacheck" %% "scalacheck" % "1.13.2" % "test",
                              "org.scalaz" %% "scalaz-core" % "7.2.4")
)

lazy val root = (project in file(".")).
  settings(foldSettings: _*).
  settings(
    name := "Applicative Folds"
  )
