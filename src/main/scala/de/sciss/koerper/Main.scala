/*
 *  Main.scala
 *  (Körper)
 *
 *  Copyright (c) 2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.koerper

import javax.swing.JFrame

import com.github.sarxos.webcam.{Webcam, WebcamPanel, WebcamResolution}

import scala.collection.JavaConverters._
import scala.swing.{Dimension, Swing}

object Main {
  def main(args: Array[String]): Unit = {
    val default = Config()
    val p = new scopt.OptionParser[Config]("Körper") {
      opt[String] ('c', "cam")
        .text ("Camera name (default: none)")
        .action   { (v, c) => c.copy(camName = Some(v)) }

      opt[Int] ('w', "width")
        .text (s"Camera capture width in pixels (default: ${default.camWidth}")
        .action   { (v, c) => c.copy(camWidth = v) }

      opt[Int] ('h', "height")
        .text (s"Camera capture height in pixels (default: ${default.camHeight}")
        .action   { (v, c) => c.copy(camHeight = v) }

      opt[Unit] ('l', "list")
        .text ("List available cameras")
        .action   { (_, c) => c.copy(listCams = true) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def run(config: Config): Unit = {
    val cam = config.camName.fold(Webcam.getDefault()) { n =>
      Webcam.getWebcams.asScala.find(_.getName == n).getOrElse(
        sys.error(s"Camera '$n' not found")
      )
    }
    cam.setViewSize(new Dimension(config.camWidth, config.camHeight))
    Swing.onEDT {
      mkGUI(cam)
    }
  }

  def mkGUI(cam: Webcam): Unit = {
    val panel = new WebcamPanel(cam)
    panel.setFPSDisplayed(true)
    panel.setDisplayDebugInfo(true)
    panel.setImageSizeDisplayed(true)
    panel.setMirrored(true)

    val window = new JFrame("Test webcam panel")
    window.add(panel)
    window.setResizable(true)
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    window.pack()
//    window.setSize(400, 400)
    window.setVisible(true)
  }
}
