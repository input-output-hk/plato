import Dependencies._

enablePlugins(JavaAppPackaging, SolidityPlugin)

val Name = "etc-client"

val commonSettings = Seq(
  name := Name,
  version := "0.1",
  scalaVersion := "2.12.1"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings"
)

testOptions in Test += Tests.Argument("-oD")

val Integration = config("it") extend Test

val Evm = config("evm") extend Test

lazy val network = project
  .configs(Integration)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= dep)
  .settings(inConfig(Integration)(Defaults.testSettings): _*)

lazy val node = project
  .dependsOn(network)
  .configs(Integration)
  .configs(Evm)
  .settings(commonSettings: _*)
  .settings(libraryDependencies ++= dep)
  .settings(inConfig(Integration)(Defaults.testSettings): _*)
  .settings(inConfig(Evm)(Defaults.testSettings): _*)

(sourceDirectory in Evm) := node.base / "src" / "evmTest"
//(test in Evm) := (test in Evm).dependsOn(solidityCompile).value

//scalastyleSources in Test ++= {(unmanagedSourceDirectories in Integration).value}

lazy val root = (project in file(".")).aggregate(network, node)

coverageExcludedPackages := "io.iohk.ethereum.vmrunner.*"
(scalastyleConfig in Test) := baseDirectory.value / "scalastyle-test-config.xml"