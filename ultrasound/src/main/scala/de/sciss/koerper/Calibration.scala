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

import de.sciss.file._
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{Graph, graph}
import de.sciss.koerper.DopplerTest.{Config, analyze, calcNumWin, calcWinStep}
import de.sciss.numbers.Implicits._
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

object Calibration {
  def main(args: Array[String]): Unit = {
//     run(Config())
//    testRead()
    testApply(Config())
  }

  def formatTemplate(f: File, args: Any*): File = {
    val name = f.name.format(args: _*)
    f.replaceName(name)
  }

  def testApply(config: Config): Unit = {
    import config._
    val tempIn    = file("/data/projects/Koerper/audio_work/us-180512-approach-continuous-motu-%d.aif")
    val fCalib    = file("/data/temp/test-calib.aif")
    val specCalb  = AudioFile.readSpec(fCalib)
//    require (specIn.sampleRate == sr)
    require (specCalb.numFrames == numBands)
    val tempOut   = file("/data/temp/test-removeQ-%d.png")

    val g = Graph {
      import graph._
      for (ch <- 4 to 5) {
        val fIn       = formatTemplate(tempIn , ch)
        val fOut      = formatTemplate(tempOut, ch)
        val specIn    = AudioFile.readSpec(fIn)
        val numWin    = calcNumWin(specIn.numFrames, config)
        val dctNum    = (numWin - (numWin%4)) >> 1
        val fftSize   = dctNum << 1
        println(s"[$ch] numWin = $numWin; numBands = $numBands; winStep = ${calcWinStep(config)}; fftSize = $fftSize")
        val in        = AudioFileIn(file = fIn    , numChannels = 1) * gainIn
        val calib     = AudioFileIn(file = fCalib , numChannels = 1)
        val calibR    = RepeatWindow(calib, size = numBands, num = numWin)
        val constQ    = analyze(in, config)
        val norm      = constQ.sqrt // / dbMax.dbAmp
        val min       = norm min calibR
        val thresh    = norm - min
        val rot       = RotateFlipMatrix(thresh, rows = numWin, columns = numBands, mode = RotateFlipMatrix.Rot90CCW)
//        val dctNum    = numWin/2
//        val dct       = DCT_II(rot, size = numWin, numCoeffs = dctNum /* numWin */)
//        val dctNum    = (numWin + 1).nextPowerOfTwo >> 2
        val rotT      = if (fftSize == numWin) rot else ResizeWindow(rot, size = numWin, stop = fftSize - numWin)

        val dct0      = Real1FFT(rotT, size = fftSize, mode = 2) * 90.50967  // gain is arbitrary
//        val dct       = dct0.complex.mag
        val dct = ConstQ(dct0, minFreqN = 0.0064, fftSize = fftSize, numBands = dctNum).sqrt * 2 // * 256 // 90.50967

        RunningMin(dct).last.poll(0, "min")
        RunningMax(dct).last.poll(0, "max")
//        Length    (in)      .poll(0, "len-in")
//        Length    (constQ)  .poll(0, "len-cq")
//        Length    (thresh)  .poll(0, "len-thresh")
//        Length    (rot)     .poll(0, "len-rot")
//        Length    (dct0)    .poll(0, "len-dct0")
//        Length    (dct)     .poll(0, "len-dct")
        val max       = dct.ampDb.linLin(dbMin, dbMax, 0.0, 1.0).clip()
        val specOut   = ImageFile.Spec(width = dctNum, height = numBands, numChannels = 1)
        ImageFileOut(file = fOut, spec = specOut, in = max)
      }
    }

    val ctrl = Control()
    ctrl.run(g)
    import ctrl.config.executionContext
    ctrl.status.foreach { _ =>
      sys.exit()
    }
  }

  def testRead(): Unit = {
    val fIn   = file("/data/temp/test-calib.aif")
    val afIn  = AudioFile.openRead(fIn)
    try {
      val buf   = afIn.buffer(afIn.numFrames.toInt)
      afIn.read(buf)
      println(buf(0).map(x => f"${x.ampDb * 0.5}%g").mkString("[", ", ", "]"))
    } finally {
      afIn.close()
    }
  }

  def run(config: Config): Unit = {
    import config._
    val fIn       = file("/data/temp/us-180512-calib-continuous-motu.aif")
    val specIn    = AudioFile.readSpec(fIn)
    require (specIn.sampleRate == sr)
    val fOut      = file("/data/temp/test-calib.aif")
//    val numWin    = calcNumWin(specIn.numFrames, config)

    val g = Graph {
      import graph._
      val in        = AudioFileIn(file = fIn, numChannels = 1) * gainIn
      val constQ    = analyze(in, config)
      val norm      = constQ.sqrt // / dbMax.dbAmp
      val median    = SlidingWindowPercentile(norm, winSize = numBands, medianLen = 63,  frac = 0.9)
      val max       = RunningWindowMax(median, size = numBands).takeRight(numBands)
      AudioFileOut(fOut, AudioFileSpec(numChannels = 1, sampleRate = sr), in = max) // sr doesn't matter
    }

    val ctrl = Control()
    ctrl.run(g)
    import ctrl.config.executionContext
    ctrl.status.foreach { _ =>
      sys.exit()
    }
  }

  def runOLD(config: Config): Unit = {
    import config._
    val fIn       = file("/data/temp/us-180512-calib-continuous-motu.aif")
    val specIn    = AudioFile.readSpec(fIn)
    require (specIn.sampleRate == sr)
    val fOut      = file("/data/temp/us-test-calib-sonogram.png")
    val numWin    = calcNumWin(specIn.numFrames, config)

    val g = Graph {
      import graph._
      val in        = AudioFileIn(file = fIn, numChannels = 1) * gainIn
      val constQ    = analyze(in, config)
      val norm      = constQ.ampDb.linLin(dbMin * 2, dbMax * 2, 0.0, 1.0).clip()
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