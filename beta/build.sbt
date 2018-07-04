lazy val baseName  = "Koerper-Beta"
lazy val baseNameL = baseName.toLowerCase

lazy val projectVersion = "0.1.0-SNAPSHOT"

def appMainClass = Some("de.sciss.koerper.KoerperBeta")

lazy val commonSettings = Seq(
  name         := baseName,
  version      := projectVersion,
  description  := "An algorithmic art project (sound installation)",
  organization := "de.sciss",
  homepage     := Some(url("https://github.com/Sciss/Koerper")),
  licenses     := Seq("gpl v3+" -> url("http://www.gnu.org/licenses/gpl-3.0.txt")),
  scalaVersion := "2.12.6",
  scalacOptions in (Compile, compile) ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint"),
  fork in run  := true
)

lazy val assemblySettings = Seq(
  // ---- assembly ----
  test            in assembly := {},
  mainClass       in assembly := appMainClass,
  target          in assembly := baseDirectory.value,
  assemblyJarName in assembly := s"$baseNameL.jar",
  assemblyMergeStrategy in assembly := {
    case "logback.xml" => MergeStrategy.last
    case PathList("org", "xmlpull", _ @ _*)              => MergeStrategy.first
    case PathList("org", "w3c", "dom", "events", _ @ _*) => MergeStrategy.first // bloody Apache Batik
    case x =>
      val old = (assemblyMergeStrategy in assembly).value
      old(x)
  },
  fullClasspath in assembly := (fullClasspath in Test).value // https://github.com/sbt/sbt-assembly/issues/27
)

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(assemblySettings)
  .settings(
    scalacOptions /* in (Compile, compile) */ += "-Yrangepos",  // this is needed to extract source code
    resolvers ++= Seq(
      "Oracle Repository" at "http://download.oracle.com/maven", // required for sleepycat
      "jzv3d releases"    at "http://maven.jzy3d.org/releases"
    ),
    libraryDependencies ++= Seq(
      "de.sciss"          %% "fileutil"                 % deps.main.fileUtil,
      "de.sciss"          %% "fscape-macros"            % deps.main.fscape,
      "de.sciss"          %% "lucre-bdb"                % deps.main.lucre,
      "de.sciss"          %% "mellite"                  % deps.main.mellite,
      "de.sciss"          %% "neuralgas-sphere"         % deps.main.neuralGas,
      "de.sciss"          %% "soundprocesses-core"      % deps.main.soundProcesses,
      "de.sciss"          %% "soundprocesses-compiler"  % deps.main.soundProcesses,
      "com.github.scopt"  %% "scopt"                    % deps.main.scopt
    )
  )

lazy val deps = new {
  val main = new {
    val fileUtil          = "1.1.3"
    val fscape            = "2.15.3"
    val lucre             = "3.8.0"
    val kollflitz         = "0.2.2"
    val mellite           = "2.24.0"
    val neuralGas         = "2.3.2"
    val scopt             = "3.7.0"
    val soundProcesses    = "3.20.2"
  }
}
