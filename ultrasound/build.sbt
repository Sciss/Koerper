lazy val baseName  = "Koerper-Ultrasound"
lazy val baseNameL = baseName.toLowerCase

lazy val projectVersion = "0.1.3"

def appMainClass = Some("de.sciss.koerper.Koerper")

lazy val commonSettings = Seq(
  name         := baseName,
  version      := projectVersion,
  description  := "An algorithmic art project (sound installation)",
  organization := "de.sciss",
  homepage     := Some(url(s"https://github.com/Sciss/${name.value}")),
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
  }
)

lazy val root = project.in(file("."))
  .settings(commonSettings)
  .settings(assemblySettings)
  .settings(
//    initialCommands in console := """import de.sciss.koerper.Geom._""",
    scalacOptions /* in (Compile, compile) */ += "-Yrangepos",  // this is needed to extract source code
    resolvers ++= Seq(
      "Oracle Repository" at "http://download.oracle.com/maven", // required for sleepycat
      "jzv3d releases"    at "http://maven.jzy3d.org/releases"
    ),
    libraryDependencies ++= Seq(
      "de.sciss"          %% "fileutil"                 % deps.main.fileUtil,
      "de.sciss"          %% "fscape-macros"            % deps.main.fscape,
      "de.sciss"          %  "intensitypalette"         % deps.main.intensityPalette,
      "de.sciss"          %  "kdtree"                   % deps.main.kdTree,
      "de.sciss"          %% "kollflitz"                % deps.main.kollflitz,
      "de.sciss"          %% "lucre-bdb"                % deps.main.lucre,
      "de.sciss"          %% "mellite"                  % deps.main.mellite,
      "de.sciss"          %% "neuralgas-sphere"         % deps.main.neuralGas,
      "de.sciss"          %% "numbers"                  % deps.main.numbers,
      "de.sciss"          %% "scalaosc"                 % deps.main.scalaOSC,
      "de.sciss"          %% "soundprocesses-core"      % deps.main.soundProcesses,
      "de.sciss"          %% "soundprocesses-compiler"  % deps.main.soundProcesses,
      "de.sciss"          %% "swingplus"                % deps.main.swingPlus,
      "com.github.scopt"  %% "scopt"                    % deps.main.scopt,
      "org.jzy3d"         %  "jzy3d-api"                % deps.main.jzy3d
    )
  )

lazy val deps = new {
  val main = new {
    val fileUtil          = "1.1.3"
    val fscape            = "2.15.3"
    val intensityPalette  = "1.0.0"
    val jzy3d             = "1.0.2"
    val kdTree            = "0.1.1"
    val kollflitz         = "0.2.2"
    val lucre             = "3.8.0"
    val mellite           = "2.23.3"
    val neuralGas         = "2.3.2"
    val numbers           = "0.2.0"
    val scalaOSC          = "1.1.6"
    val scopt             = "3.7.0"
    val soundProcesses    = "3.20.2"
    val swingPlus         = "0.3.0"
  }
}
