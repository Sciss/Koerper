/*
 *  DopplerTest.scala
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

import de.sciss.file._
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph, graph}
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.AudioFile

object DopplerTest {

  case class Config(dbMin: Double = -66.0, dbMax: Double = -18.0,
                    freqMin: Double = 39500.0, freqMax: Double = 40000.0.squared / 39500.0,
                    numBands: Int = 384, fftSize: Int = 8192, timeResMS: Double = 4.0, gainIn: Double = 40.0,
                    sr: Double = 96000.0)

  def main(args: Array[String]): Unit = {
    run(Config())
  }

  def analyze(in: GE, config: Config): GE = {
    import graph._
    import config._
    val winStep = math.min(fftSize, (timeResMS / 1000 * sr + 0.5).toInt)
    val minFreqN = freqMin / sr
    val maxFreqN = freqMax / sr

    val slid = Sliding(in, fftSize, winStep)
    val winFun = GenWindow(size = fftSize, shape = GenWindow.Hann)
    val windowed = slid * winFun
    val rotWin = RotateWindow(windowed, size = fftSize, amount = fftSize / 2)
    val fft = Real1FFT(rotWin, size = fftSize)
    val constQ = ConstQ(fft, fftSize = fftSize, minFreqN = minFreqN, maxFreqN = maxFreqN, numBands = numBands)
    val norm = constQ.ampDb.linLin(dbMin * 2, dbMax * 2, 0.0, 1.0).clip()
    norm
  }

  def calcWinStep(config: Config): Int = {
    import config._
    math.min(fftSize, (timeResMS / 1000 * sr + 0.5).toInt)
  }

  def calcNumWin(numFrames: Long, config: Config): Int = {
    import config._
    val winStep   = calcWinStep(config)
    val numWin    = ((numFrames - fftSize + winStep - 1) / winStep).toInt
    numWin
  }

  def run(config: Config): Unit = {
    import config._
    val fIn       = file("/data/projects/Koerper/audio_work/us-180512-approach-continuous-motu-1.aif")
    val specIn    = AudioFile.readSpec(fIn)
    require (specIn.sampleRate == sr)
    val fOut      = file("/data/temp/us-test-sonogram.png")
    val numWin    = calcNumWin(specIn.numFrames, config)

    val g = Graph {
      import graph._
      val in        = AudioFileIn(file = fIn, numChannels = 1) * gainIn
      val norm      = analyze(in, config)
      val rotImg    = RotateFlipMatrix(norm, rows = numWin, columns = numBands, mode = RotateFlipMatrix.Rot90CCW)
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
