name         := "Koerper-Ultrasound"
version      := "0.1.0-SNAPSHOT"
description  := "An algorithmic art project (sound installation)"
organization := "de.sciss"
homepage     := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses     := Seq("gpl v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))
scalaVersion := "2.12.6"

scalacOptions in (Compile, compile) ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint")

initialCommands in console := """import de.sciss.koerper.Geom._"""

libraryDependencies ++= Seq(
  "de.sciss"          %% "fileutil"         % deps.main.fileUtil,
  "de.sciss"          %% "fscape-core"      % deps.main.fscape,
  "de.sciss"          %% "kollflitz"        % deps.main.kollflitz,
  "de.sciss"          %  "neuralgas-core"   % deps.main.neuralGas,
  "de.sciss"          %% "numbers"          % deps.main.numbers,
  "de.sciss"          %% "scalaosc"         % deps.main.scalaOSC,
  "de.sciss"          %% "swingplus"        % deps.main.swingPlus,
  "de.sciss"          %  "intensitypalette" % deps.main.intensityPalette,
  "de.sciss"          %  "kdtree"           % deps.main.kdTree,
  "com.github.scopt"  %% "scopt"            % deps.main.scopt
)

lazy val deps = new {
  val main = new {
    val fileUtil          = "1.1.3"
    val fscape            = "2.15.1"
    val intensityPalette  = "1.0.0"
    val kdTree            = "0.1.1"
    val kollflitz         = "0.2.2"
    val neuralGas         = "2.3.1"
    val numbers           = "0.1.5"
    val scalaOSC          = "1.1.6"
    val scopt             = "3.7.0"
    val swingPlus         = "0.3.0"
  }
}
