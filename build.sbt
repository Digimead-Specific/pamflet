//
// Copyright (c) 2013 Alexey Aksenov ezh@ezh.msk.ru
//
// Booklet is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Booklet is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Booklet. If not, see <http://www.gnu.org/licenses/>.

// DEVELOPMENT CONFIGURATION

val unfilteredVersion = "0.7.0"
val stringtemplateVersion = "3.2.1"
val launcherInterfaceVersion = "0.13.0"
val servletApiVersion = "2.5"

val commonSettings = Seq(
    name := "booklet",
    organization := "org.digimead",
    version <<= (baseDirectory in LocalRootProject) { (b) => scala.io.Source.fromFile(b / "version").mkString.trim },
    //
    // Dependencies
    //
    resolvers += "digimead-maven" at "http://storage.googleapis.com/maven.repository.digimead.org/",
    resolvers += "sonatype-releases" at "https://oss.sonatype.org/service/local/repositories/releases/content/",
    libraryDependencies += "com.tristanhunt" %% "knockoff" % "0.8.1",
    libraryDependencies += "net.databinder" %% "unfiltered-filter" % unfilteredVersion,
    libraryDependencies += "net.databinder" %% "unfiltered-jetty" % unfilteredVersion,
    libraryDependencies += "org.antlr" % "stringtemplate" % stringtemplateVersion,
    libraryDependencies += "org.jsoup" % "jsoup" % "1.7.3",
    libraryDependencies += "org.fusesource.scalate" %% "scalate-core" % "1.6.1",
    libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.5")

lazy val booklet: Project = (project in file(".")) settings(commonSettings: _*) settings(
    name := "booklet",
    publishArtifact := false) aggregate(knockoff, library, app)
lazy val knockoff: Project = (project in file("booklet-knockoff")) settings(commonSettings: _*) settings(
    name := "booklet-knockoff",
    description := "Extensions to the Knockoff Markdown parser")
lazy val library: Project = (project in file("booklet-library")) settings(commonSettings: _*) settings(
    name := "booklet-library",
    description := "Core booklet library") dependsOn(knockoff)
lazy val app: Project = (project in file("booklet-app")) settings(commonSettings: _*) settings(
    name := "booklet-app",
    description := "booklet app for previewing and publishing project documentation",
    libraryDependencies += "org.scala-sbt" % "launcher-interface" % launcherInterfaceVersion % "provided",
    libraryDependencies += "javax.servlet" % "servlet-api" % servletApiVersion,
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.5") dependsOn(library)

//logLevel := Level.Debug
