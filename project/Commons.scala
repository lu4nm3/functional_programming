import sbt.Keys._
import sbt._

object Commons {
  val _version = "1.0"
  val _scalacOptions = Seq("-feature")
  val _scalaVersion = "2.11.6"

  val settings: Seq[Def.Setting[_]] = Seq(
    version := _version,
    scalacOptions := _scalacOptions,
    scalaVersion := _scalaVersion
  )
}