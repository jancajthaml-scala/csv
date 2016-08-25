name := "csv"

version := "0.1.0-SNAPSHOT"

description := "csv parser"

scalaVersion := "2.11.8"

lazy val demo = (project in file(".")).settings(
  name := "CSV parser",
  mainClass in Compile := Some("com.github.jancajthaml.csv.Main")
)