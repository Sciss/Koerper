/*
 *  Calibration.scala
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

import de.sciss.file.file
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{Graph, graph}
import de.sciss.koerper.DopplerTest.{Config, analyze, calcNumWin}
import de.sciss.synth.io.AudioFile

object Calibration {
  def main(args: Array[String]): Unit = {
    run(Config())
  }

  def run(config: Config): Unit = {
    import config._
    val fIn       = file("/data/temp/us-180512-calib-continuous-motu.aif")
    val specIn    = AudioFile.readSpec(fIn)
    require (specIn.sampleRate == sr)
    val fOut      = file("/data/temp/us-test-calib-sonogram.png")
    val numWin    = calcNumWin(specIn.numFrames, config)

    val g = Graph {
      import graph._
      val in        = AudioFileIn(file = fIn, numChannels = 1) * gainIn
      val norm      = analyze(in, config)
      val median    = SlidingWindowPercentile(norm, winSize = numBands, medianLen = 63,  frac = 0.9)
      val max       = RunningWindowMax(median, size = numBands)

      val rotImg    = RotateFlipMatrix(max, rows = numWin, columns = numBands, mode = RotateFlipMatrix.Rot90CCW)
      val specOut   = ImageFile.Spec(width = numWin, height = numBands, numChannels = 1)
      ImageFileOut(file = fOut, spec = specOut, in = rotImg)
    }

    val ctrl = Control()
    ctrl.run(g)
    import ctrl.config.executionContext
    ctrl.status.foreach { _ =>
      sys.exit()
    }
  }

}
