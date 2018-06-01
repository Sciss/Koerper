/*
 *  RenderProbabilityDistribution.scala
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

import de.sciss.fscape.GE
import de.sciss.fscape.lucre.{FScape, MacroImplicits}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.synth.proc.Implicits._

object RenderProbabilityDistribution {
  final val KeyAudioIn    = "audio-in"
  final val TKeyVoronoiIn = "voronoi-in-%d"
  final val TKeySphereIn  = "sphere-in-%d"
  final val KeyCalibIn    = "calib-in"
  final val KeyTableOut   = "table-out"

  final val ValueName     = "render-pd"

  def mkObj[S <: Sys[S]](config: Obj[S])(implicit tx: S#Tx): FScape[S] = {
    import MacroImplicits._
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.lucre.graph._

    val f = FScape[S]
    f.setGraph {
      val freqMin     = "freq-min"    .attr(39500.0)
      val freqMax     = "freq-max"    .attr(40000.0 * 40000.0 / 39500.0)
      val sr          = "sample-rate" .attr(96000.0)
      val fftSize     = "fft-size"    .attr(8192)
      val numBands    = "num-bands"   .attr(384)
      val timeResMS   = "time-res"    .attr(4.0)
      val gainIn      = "gain-in"     .attr(40.0)
      val dbMin       = "db-min"      .attr(-66.0)
      val dbMax       = "db-max"      .attr(-18.0)

      // returns squared coefficients
      def analyze(in: GE): GE = {
        val winStep   = calcWinStep()
        val minFreqN  = freqMin / sr
        val maxFreqN  = freqMax / sr
        val slid      = Sliding(in, fftSize, winStep)
        val winFun    = GenWindow(size = fftSize, shape = GenWindow.Hann)
        val windowed  = slid * winFun
        val rotWin    = RotateWindow(windowed, size = fftSize, amount = fftSize / 2)
        val fft       = Real1FFT(rotWin, size = fftSize)
        val constQ    = ConstQ(fft, fftSize = fftSize, minFreqN = minFreqN, maxFreqN = maxFreqN, numBands = numBands)
        constQ
      }

      def calcWinStep(): GE = {
        fftSize min (timeResMS / 1000 * sr + 0.5).floor
      }

      def calcNumWin(numFrames: GE): GE = {
        import de.sciss.fscape._
        val winStep   = calcWinStep()
        val numWin    = ((numFrames + winStep - 1) / winStep).floor
        numWin
      }

      def inAll     = AudioFileIn("audio-in")   // `def` because the channels will be concatenated
//      def calibAll  = AudioFileIn("calib-in")   // dito
      // XXX TODO testing
      def calibAll  = de.sciss.fscape.graph.AudioFileIn(new java.io.File("/data/temp/test-calib.aif"), numChannels = 1)

      val numWin0   = calcNumWin(inAll.numFrames)
      val numWin    = numWin0 min 8192

      val outSeq = for (ch <- 0 until 5) yield {
        val in0       = inAll.out(ch)
        val in        = in0 * gainIn
        val calib     = calibAll.out(ch)
        val calibR    = RepeatWindow(calib, size = numBands, num = numWin)
        val constQ    = analyze(in)
        val norm      = constQ.sqrt
        val min       = norm min calibR
        val thresh    = norm - min
        val rot       = thresh
        val max       = rot.ampDb.linLin(dbMin, dbMax, 0.0, 1.0).clip()
        val coordV    = AudioFileIn("voronoi-in-%d".format(ch+1))
        val coordSph  = AudioFileIn("sphere-in-%d" .format(ch+1))
        val posH      = coordV.out(0) * numWin
        val posV      = coordV.out(1) * numBands
        val scanned   = ScanImage(max, width = numBands, height = numWin, x = posV, y = posH, zeroCrossings = 0)
        val isZero    = scanned sig_== 0.0
        val dataF     = FilterSeq(scanned         , isZero)
        val thetaF    = FilterSeq(coordSph.out(0) , isZero)
        val phiF      = FilterSeq(coordSph.out(1) , isZero)

        (dataF, thetaF, phiF)
      }

      val (outData, outTheta, outPhi) = outSeq.unzip3

      def cat(seq: Seq[GE]): GE = seq.reduceLeft(_ ++ _)

      val outInteg  = RunningSum(cat(outData))
      val outSig    = Seq(outInteg, cat(outTheta), cat(outPhi)): GE

      /* val framesOut = */ AudioFileOut("table-out", in = outSig)
    }

//    f.graph() = g
    val a     = f.attr
    val ca    = config.attr

    // copy the configuration which is the attribute map of the `config` object
    ConstQConfig.keys.foreach { key =>
      ca.get(key).foreach(value => a.put(key, value))
    }

    f.name = ValueName

    f
  }
}
