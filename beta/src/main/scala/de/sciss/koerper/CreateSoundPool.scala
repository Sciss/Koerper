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
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.control.NonFatal

object CreateSoundPool {
  def main(args: Array[String]): Unit = {
//    findAllSpots()
    findEmptySpots()
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

  val tempSpotOut: File = file("/data") / "projects" / "Koerper" / "audio_work" / "beta-spots" / "beta-spot-%d.aif"

  def findEmptySpots(): Unit = {
    val minLen = (2.5 * 44100).toLong
    var bad = Set.empty[Int]
    Iterator.from(1).map(formatTemplate(tempSpotOut, _)).takeWhile(_.exists()).zipWithIndex.foreach { case (f, idx) =>
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
}
