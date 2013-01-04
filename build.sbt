
import java.io.File
import sbt._
import Process._
import Keys._

libraryDependencies += "joda-time"  % "joda-time"     % "2.1"

libraryDependencies += "org.joda"   % "joda-convert"  % "1.2"  

name := "DartsLibJSON"

version := "0.1"

scalaVersion := "2.9.2"

scalaSource in Compile <<= baseDirectory (_ / "src")

