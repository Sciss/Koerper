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
package proto

import de.sciss.file._
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{GE, Graph, graph}
import de.sciss.synth.io.AudioFile

object DopplerTest {

  def main(args: Array[String]): Unit = {
    run(ConstQConfig())
  }

  // returns squared coefficients
  def analyze(in: GE, config: ConstQConfig): GE = {
    import config._
    import graph._
    val winStep   = calcWinStep(config)
    val minFreqN  = freqMin / sr
    val maxFreqN  = freqMax / sr
    val slid      = Sliding(in, fftSize, winStep)
//    Length(slid).poll(0, "len-slid")
    val winFun    = GenWindow(size = fftSize, shape = GenWindow.Hann)
    val windowed  = slid * winFun
    val rotWin    = RotateWindow(windowed, size = fftSize, amount = fftSize / 2)
//    Length(rotWin).poll(0, "len-rot-win")
    val fft       = Real1FFT(rotWin, size = fftSize)
//    Length(fft).poll(0, "len-fft")
    val constQ    = ConstQ(fft, fftSize = fftSize, minFreqN = minFreqN, maxFreqN = maxFreqN, numBands = numBands)
    constQ
  }

  def calcWinStep(config: ConstQConfig): Int = {
    import config._
    math.min(fftSize, (timeResMS / 1000 * sr + 0.5).toInt)
  }

  def calcNumWin(numFrames: Long, config: ConstQConfig): Int = {
    val winStep   = calcWinStep(config)
    val numWin    = ((numFrames /* - fftSize */ + winStep - 1) / winStep).toInt
    numWin
  }

//  private def any2stringadd(x: Any): Nothing = throw new NotImplementedError()

  def calcNumWin(numFrames: GE, config: ConstQConfig): GE = {
    import de.sciss.fscape._
    val winStep   = calcWinStep(config)
    val numWin    = ((numFrames + winStep - 1) / winStep).floor // toInt
    numWin
  }

  def run(config: ConstQConfig): Unit = {
    import config._
    val fIn       = file("/data/projects/Koerper/audio_work/TestRec180517-cordial-neu1.aif")
    val specIn    = AudioFile.readSpec(fIn)
    require (specIn.sampleRate == sr)
    val fOut      = file("/data/temp/us-test-cordial-sonogram.png")
    val numWin    = calcNumWin(specIn.numFrames, config)

    val g = Graph {
      import graph._
      val in        = AudioFileIn(file = fIn, numChannels = 1) * gainIn
      val constQ    = analyze(in, config)
      val norm      = constQ.ampDb.linLin(dbMin * 2, dbMax * 2, 0.0, 1.0).clip()
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
