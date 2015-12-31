import sbt._

object Dependencies {
  // Versions
  val akkaVersion = "2.3.9"
  val kafkaVersion = "0.8.2.0"

  // Libraries
  val akka = "com.typesafe.akka" %% "akka-actor" % akkaVersion withSources() withJavadoc()
  val kafka = "org.apache.kafka" %% "kafka" % kafkaVersion withSources() withJavadoc()

  // Dependencies
  val coreDependencies = akka
  val kafkaDependencies = kafka
}