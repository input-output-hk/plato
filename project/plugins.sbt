logLevel := sbt.Level.Warn

resolvers += Resolver.sonatypeRepo("snapshots")

addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "1.5.0")
addSbtPlugin("org.scoverage" %% "sbt-coveralls" % "1.1.0")
// ScalaStyle for multiprojects was fixed but it's still in a snapshot version
// https://github.com/scalastyle/scalastyle-sbt-plugin/pull/45
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.9.0-SNAPSHOT")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.0.6")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "latest.release")
