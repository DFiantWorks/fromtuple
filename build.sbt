val scala3Version = "3.3.4"

//Dependency versions
val munitVersion = "1.0.3"

import xerial.sbt.Sonatype.sonatypeCentralHost
//Publishing
inThisBuild(
  List(
    organization := "com.github.dfiantworks",
    homepage := Some(url("https://github.com/dfiantworks/fromtuple")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "soronpo",
        "Oron Port",
        "",
        url("https://x.com/soronpo")
      )
    ),
    versionScheme := Some("semver-spec"),
    sonatypeCredentialHost := sonatypeCentralHost
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "fromtuple",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:strictEquality",
      "-language:implicitConversions"
    ),
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
  )
