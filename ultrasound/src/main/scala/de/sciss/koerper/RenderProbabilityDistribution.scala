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
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.MacroImplicits._
import de.sciss.synth.proc.{AudioCue, Ensemble, Folder, Proc}

object RenderProbabilityDistribution {
  final val KeyAudioIn    = "audio-in"
  final val TKeyVoronoiIn = "voronoi-in-%d"
  final val TKeySphereIn  = "sphere-in-%d"
  final val KeyCalibIn    = "calib-in"
  final val KeyTableOut   = "table-out"

  final val ValueName     = "render-pd"

  def mkObjects[S <: Sys[S]](config: Obj[S], locBase: ArtifactLocation[S])(implicit tx: S#Tx): List[Obj[S]] = {
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
      def calibAll  = AudioFileIn("calib-in")   // dito

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
        val nonZero   = scanned sig_!= 0.0
        val dataF     = FilterSeq(scanned         , nonZero)
        val thetaF    = FilterSeq(coordSph.out(0) , nonZero)
        val phiF      = FilterSeq(coordSph.out(1) , nonZero)

        (dataF, thetaF, phiF)
      }

      val (outData0, outTheta0, outPhi0) = outSeq.unzip3

      def cat(seq: Seq[GE]): GE = seq.reduceLeft(_ ++ _)

      val outData   = cat(outData0  )
      val outTheta  = cat(outTheta0 )
      val outPhi    = cat(outPhi0   )

      // we'll integrate later when we have double precision?
      val outInteg  = outData // RunningSum(outData)
      val outSig    = Seq(outInteg, outTheta, outPhi): GE

      /* val framesOut = */ AudioFileOut("table-out", in = outSig)

      OnComplete("done")
    }

//    f.graph() = g
    val a     = f.attr
    val ca    = config.attr

    // copy the configuration which is the attribute map of the `config` object
    ConstQConfig.keys.foreach { key =>
      ca.get(key).foreach(value => a.put(key, value))
    }

    for (ch <- 0 until Koerper.numChannels) {
      val artV    = Artifact(locBase, Artifact.Child(s"voronoi_coord-%d.aif".format(ch+1)))
      val artSph  = Artifact(locBase, Artifact.Child(s"sphere_coord-%d.aif" .format(ch+1)))
      a.put(TKeyVoronoiIn.format(ch+1), mkCue(artV  ))
      a.put(TKeySphereIn .format(ch+1), mkCue(artSph))
    }

    // XXX TODO: Testing
    val locTemp       = ArtifactLocation.newConst(new java.io.File("/data/temp/"))
    val artCalibTemp  = Artifact(locTemp, Artifact.Child("test-calib.aif"))
    a.put(KeyCalibIn, mkCue(artCalibTemp))

    val aDone = mkDoneAction[S]()
    a.put("done", aDone)

    f.name = ValueName

    f :: Nil
  }

  private def mkCue[S <: Sys[S]](a: Artifact[S])(implicit tx: S#Tx): AudioCue.Obj[S] = {
    val artVal = a.value
    val spec   = de.sciss.synth.io.AudioFile.readSpec(artVal)
    val cue    = AudioCue.Obj(a, spec, offset = 0L, gain = 1.0)
    cue
  }

  private def mkDoneAction[S <: Sys[S]]()(implicit tx: S#Tx): Obj[S] = {
    val a = proc.Action.apply[S] { u =>
      import de.sciss.fscape.lucre.FScape
      import de.sciss.fscape.stream.Control
      import de.sciss.synth.proc.GenContext

      // store the chunk in the 'us' folder
      val folderPD  = u.root ![Folder] "pd"
      val fsc       = u.root ![FScape] "render-pd"
      val artTab    = fsc.attr ![Artifact] "table-out"
      val artTabVal = artTab.value
      val specTab   = de.sciss.synth.io.AudioFile.readSpec(artTabVal)
      val cueTab    = AudioCue.Obj(artTab, specTab, offset = 0L, gain = 1.0)
      cueTab.name   = artTabVal.getName
      folderPD.addLast(cueTab)

      println("Yo Chuck!")
    }

    a.name = "pd-done"
    a
  }
}
