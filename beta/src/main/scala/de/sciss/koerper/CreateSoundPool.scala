/*
 *  CreateSoundPool.scala
 *  (Körper)
 *
 *  Copyright (c) 2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.koerper

import de.sciss.file._
import de.sciss.numbers
import de.sciss.synth.io.{AudioFile, AudioFileSpec, SampleFormat}

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

object CreateSoundPool {
  def main(args: Array[String]): Unit = {
//    findAllSpots()
//    findEmptySpots()

    mkAllMinPhase()
  }

  def mkInputIterator(): Iterator[File] = {
    val url = this.getClass.getResource("/InputSounds.txt")
    io.Source.fromURL(url, "UTF-8").getLines().map(file)
  }

  def validateList(): Unit = {
    val minFrames = 18 * 44100L
    val map       = mutable.Map.empty[Long, List[File]]
    mkInputIterator().foreach { f =>
      try {
        val spec  = AudioFile.readSpec(f)
        if (spec.numChannels < 0) {
          println(s"NO CHANNELS: $f")
        } else if (spec.numFrames < minFrames) {
          println(s"TOO SHORT: $f")
        } else {
          val others = map.getOrElse(spec.numFrames, Nil)
          if (others.exists { that =>
            val a = AudioFile.openRead(f)
            try {
              val b = AudioFile.openRead(that)
              try {
                val ba = a.buffer(44100)
                a.read(ba)
                val bb = b.buffer(44100)
                b.read(bb)
                ba(0) sameElements bb(0)

              } finally {
                 b.cleanUp()
              }
            } finally {
              a.cleanUp()
            }
          }) {
            println(s"DUPLICATE: $f")
          } else {
            map.put(spec.numFrames, f :: others)
          }
        }
      } catch {
        case NonFatal(_) => println(s"INVALID FILE: $f")
      }
    }
  }

  final val minCutDur   = 5.0
  final val maxCutDur   = 8.0

  def findSpotsTest(): Unit = {
    val fIn     = file("/media/hhrutz/Mnemo3/hh_sounds/MiscZeven_Fahrrad.aif")
    val cutDur  = 6.0
    val fOut    = List.tabulate(3)(i => file(s"/data/temp/test-${i + 1}.aif"))
    val fut     = extractSpots(fIn = fIn, fOut = fOut, cutDur = cutDur)
    Await.result(fut, Duration.Inf)
    println("Done.")
  }

  def minPhaseTest(): Unit = {
    val fIn     = file("/data/projects/Koerper/audio_work/beta-spots/beta-spot-1.aif")
    val fOut    = file("/data/temp/test-min-phase.aif")
    val fut     = mkMinPhase(fIn = fIn, fOut = fOut)
    Await.result(fut, Duration.Inf)
    println("Done.")
  }

  val baseDir: File  = {
    val tmp = file("/data") / "projects"
    val prj = if (tmp.exists()) tmp else userHome / "Documents" / "projects"
    prj / "Koerper"
  }

  val audioWork   : File  = baseDir / "audio_work"
  val tempSpotOut : File  = audioWork / "beta-spots" / "beta-spot-%d.aif"
  val tempPhaseOut: File  = audioWork / "beta-phase" / "beta-phase-%d.aif"
  val tempFeat    : File  = audioWork / "beta-feat"  / "beta-feat-%d.bin"

  def mkSpotIterator(): Iterator[(File, Int)] =
    Iterator.from(1).map(formatTemplate(tempSpotOut, _)).takeWhile(_.exists()).zipWithIndex

  def mkPhaseIterator(): Iterator[(File, Int)] =
    Iterator.from(1).map(formatTemplate(tempPhaseOut, _)).takeWhile(_.exists()).zipWithIndex

  def mkFeatIterator(): Iterator[(File, Int)] =
    Iterator.from(1).map(formatTemplate(tempFeat, _)).takeWhile(_.exists()).zipWithIndex

  def findEmptySpots(): Unit = {
    val minLen = (2.5 * 44100).toLong
    var bad = Set.empty[Int]
    mkSpotIterator().foreach { case (f, idx) =>
      val idxIn = idx/3 + 1
      val n = s"${f.name} ($idxIn)"
      def bail(msg: String): Unit = {
        bad += idxIn
        println(msg)
      }
      try {
        val af = AudioFile.openRead(f)
        try {
          if (af.numFrames < minLen) {
            bail(s"TOO SHORT: $n")
          } else {
            val len = af.numFrames.toInt
            val b = af.buffer(len)
            af.read(b)
            import numbers.Implicits._
            val rms = (b(0).iterator.map(_.squared).sum / len).sqrt.ampDb
            if (rms < -80) {
              bail(f"TOO QUIET ($rms%g dB RMS): $n")
            } else if (rms > -3) {
              bail(f"TOO LOUD ($rms%g dB RMS): $n")
            } else if (rms.isNaN) {
              bail(s"HAS NANS: $n")
            }
          }
        } finally {
          af.cleanUp()
        }
      } catch {
        case NonFatal(_) =>
          bail(s"NOT READABLE: $n")
      }
    }

    println(bad.toList.sorted.mkString("\nBAD SET: ", ", ", ""))
  }

  def findAllSpots(): Unit = {
    val pqSize  = 3
    val rnd     = new scala.util.Random(2187)
    mkInputIterator().zipWithIndex.foreach { case (fIn, idx) =>
      val fOut = List.tabulate(pqSize) { sliceIdx =>
        val idxOut = idx * pqSize + sliceIdx + 1
        formatTemplate(tempSpotOut, idxOut)
      }
      import numbers.Implicits._
      val cutDur = rnd.nextDouble().linLin(0, 1, minCutDur, maxCutDur)
      if (fOut.exists(!_.exists())) {
        val fut     = extractSpots(fIn = fIn, fOut = fOut, cutDur = cutDur)
        Await.result(fut, Duration.Inf)
        println(s"Done ${idx + 1}.")
      }
    }
  }

  def mkAllMinPhase(): Unit = {
    mkSpotIterator().foreach { case (fIn, idx) =>
      val fOut = formatTemplate(tempPhaseOut, idx + 1)
      if (!fOut.exists()) {
        val fut = mkMinPhase(fIn = fIn, fOut = fOut)
        Await.result(fut, Duration.Inf)
        println(s"Done ${idx + 1}.")
      }
    }
  }

  def formatTemplate(f: File, args: Any*): File = {
    val name = f.name.format(args: _*)
    f.replaceName(name)
  }

  def extractSpots(fIn: File, fOut: List[File], cutDur: Double): Future[Unit] = {
    require(cutDur >= minCutDur && cutDur <= maxCutDur)

    import de.sciss.fscape._
    import graph._

//    val fIn     = file("/media/hhrutz/Mnemo3/hh_sounds/Betanovuss150410_1.aif")
    val specIn  = AudioFile.readSpec(fIn)

    val g = Graph {
      val sr          = 44100.0
      val highPass    = 100.0

      val numFrames   = math.min(specIn.numFrames, (sr * 60 * 30).toLong) // 30 minutes
      def mkIn(): GE = {
        val in0         = AudioFileIn(fIn, numChannels = specIn.numChannels)
        val in1         = if (specIn.numChannels == 1) in0 else in0.out(0)
        val in2         = if (specIn.numFrames == numFrames) in1 else in1.take(numFrames)
        HPF(in2, highPass / sr)
      }
      val in          = mkIn()
      val winSize     = sr.toInt
      val stepSize    = winSize / 4
      val lap         = Sliding(in, winSize, stepSize)
      val loud        = Loudness(lap, sampleRate = sr, size = winSize)
      val numLoud     = ((numFrames + stepSize - 1) / stepSize).toInt
      val maxSize     = (sr * 3 / stepSize).toInt
      val dropRight   = (sr * maxCutDur / stepSize).toInt
      val takeLoud    = numLoud - dropRight
      val isLocalMax  = DetectLocalMax(loud, size = maxSize).take(takeLoud)
      val loudIdx     = Frames(loud)
      val pqSize      = fOut.size // 3
      val pq          = PriorityQueue(keys = loud * isLocalMax, values = loudIdx, size = pqSize)
      val offsets     = pq * stepSize

      for (sliceIdx <- 0 until pqSize) {
        val inCopy    = mkIn()
        val start     = {
          val tmp = if (sliceIdx == 0) offsets else offsets.drop(sliceIdx)
          tmp.take(1)
        }
//        start.poll(0, s"start-${sliceIdx + 1}")
        val stop      = start + (cutDur * sr).toLong
        val slice     = Slices(inCopy, spans = start ++ stop)
        val sig       = slice // XXX TODO --- create minimum phase version
        val fOutI     = fOut(sliceIdx)
        AudioFileOut(fOutI, AudioFileSpec(numChannels = 1, sampleRate = sr), in = sig)
      }
    }

    val ctl = stream.Control()
    ctl.run(g)
    ctl.status
  }

  def mkMinPhase(fIn: File, fOut: File): Future[Unit] = {
    import numbers.Implicits._
    import de.sciss.fscape._
    import graph._

    val specIn  = AudioFile.readSpec(fIn)

    val g = Graph {
      val sr          = 44100.0
      val highPass    = 50.0
      val numFramesIn = specIn.numFrames.toInt
      val fftSize     = numFramesIn.nextPowerOfTwo * 2
      val in          = AudioFileIn(fIn, numChannels = 1) // ++ DC(0).take(fftSize - specIn.numFrames)
      val inR         = HPF(in, highPass/sr) // RotateWindow(in, fftSize, fftSize/2)
      val fft         = Real1FullFFT(in = inR, size = fftSize)
      val logC0       = fft.complex.log // .max(-100) // (-80)

      // unwrap phases is _not_ needed

//      val logC = {
//        val mag     = logC0.complex.real
//        val phase0  = logC0.complex.phase
//        val phaseD  = Biquad(phase0, b0 = 1, b1 = -1)
//        val dn      = phaseD > +math.Pi
//        val up      = phaseD < -math.Pi
//        val phaseA  = up * math.Pi - dn * math.Pi
//        val phase   = phase0 + phaseA
//        mag zip phase
//      }

      val logC = logC0

      val cep         = Complex1IFFT(in = logC, size = fftSize) / fftSize
      val cepOut      = FoldCepstrum(in = cep, size = fftSize,
        crr = +1, cri = +1, clr = 0, cli = 0,
        ccr = +1, cci = -1, car = 0, cai = 0)

     val freq        = Complex1FFT(in = cepOut, size = fftSize) * fftSize
      val fftOut      = freq.complex.exp

      val outW        = Real1FullIFFT (in = fftOut, size = fftSize)
      val outWR       = outW.take(numFramesIn) // RotateWindow(outW, fftSize, fftSize/2)
      val convSize    = (numFramesIn + numFramesIn - 1).nextPowerOfTwo
      val conv1       = Real1FFT(outWR, size = convSize, mode = 1)
      val conv2       = conv1.complex.squared
      val conv3       = Real1IFFT(conv2, size = convSize, mode = 1)
//      val conv3       = outWR
      val outLen      = numFramesIn + numFramesIn - 1
      val convOut     = conv3.take(outLen)

      def normalize(in: GE): GE = {
        val max       = RunningMax(in.abs).last
//        max.ampDb.poll(0, "max [dB]")
        val headroom  = -0.2.dbAmp
        val gain      = max.reciprocal * headroom
        val buf       = BufferDisk(in)
        val sig0      = buf * gain
        val spl       = 55
        val ref       = 32
//        val loud      = Loudness(sig0, sampleRate = sr, size = outLen, spl = spl)
        val loud0     = Loudness(sig0, sampleRate = sr, size = sr/4, spl = spl)
        val loud      = RunningMax(loud0).last
//        Length(loud).poll(0, "NUM-LOUD")
        loud.poll(0, s"LOUD-0 $fOut")
        val buf1      = BufferDisk(buf)
        val gain1     = (-(loud.max(ref)) + ref).dbAmp.pow(0.6) * gain  // 0.7 -- some good guess for phon <-> dB
//        gain1.poll(0, s"GAIN $fOut")
        val sig       = buf1 * gain1
        val loud00    = Loudness(sig, sampleRate = sr, size = sr/4, spl = spl)
        val loud1     = RunningMax(loud00).last
//        val loud1     = Loudness(sig, sampleRate = sr, size = outLen, spl = spl)
        loud1.poll(0, s"LOUD-1 $fOut")
        sig
      }

      val sig         = normalize(convOut)

      val specOut     = AudioFileSpec(numChannels = 1, sampleRate = sr, sampleFormat = SampleFormat.Int24)
      AudioFileOut(fOut, specOut, in = sig)
    }

    val ctl = stream.Control()
    ctl.run(g)
    ctl.status
  }
}
