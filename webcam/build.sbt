name         := "Koerper-WebCam"
version      := "0.1.0-SNAPSHOT"
description  := "An algorithmic art project (sound installation)"
organization := "de.sciss"
homepage     := Some(url(s"https://github.com/Sciss/${name.value}"))
licenses     := Seq("gpl v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt"))
scalaVersion := "2.12.5"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xfuture", "-encoding", "utf8", "-Xlint")

libraryDependencies ++= Seq(
  "de.sciss"          %% "fileutil"       % deps.main.fileUtil,
  "de.sciss"          %% "swingplus"      % deps.main.swingPlus,
  "de.sciss"          %% "numbers"        % deps.main.numbers,
  "de.sciss"          %% "kollflitz"      % deps.main.kollflitz,
  "de.sciss"          %% "scalaosc"       % deps.main.scalaOSC,
  "de.sciss"          %  "neuralgas-core" % deps.main.neuralGas,
  "com.github.scopt"  %% "scopt"          % deps.main.scopt,
  "com.github.sarxos" %  "webcam-capture"                 % deps.main.webcam, // cam capture
  "com.github.sarxos" %  "webcam-capture-driver-openimaj" % deps.main.webcam  // usb webcam drivers
    exclude ("net.billylieurance.azuresearch" , "azure-bing-search-java")
    exclude ("uk.ac.ed.ph.snuggletex"         , "snuggletex-core")
    exclude ("uk.ac.ed.ph.snuggletex"         , "snuggletex-upconversion")
    exclude ("uk.ac.ed.ph.snuggletex"         , "snuggletex-jeuclid")
    exclude ("com.aetrion.flickr"             , "flickrapi")
    exclude ("vigna.dsi.unimi.it"             , "jal")
    exclude ("com.nativelibs4java"            , "bridj"),
  "com.nativelibs4java"       % "bridj"        % deps.main.bridj,   // needed by webcam
  "com.twelvemonkeys.imageio" % "imageio-core" % deps.main.imageio, // needed by webcam
  "com.twelvemonkeys.common"  % "common-lang"  % deps.main.imageio  // needed by webcam
)

lazy val deps = new {
  val main = new {
    val fileUtil   = "1.1.3"
    val swingPlus  = "0.3.0"
    val numbers    = "0.1.5"
    val kollflitz  = "0.2.2"
    val scalaOSC   = "1.1.6"
    val neuralGas  = "2.3.1"
    val scopt      = "3.7.0"
    val webcam     = "0.3.12"
    val bridj      = "0.7.0"
    val imageio    = "3.1.2"
  }
}
