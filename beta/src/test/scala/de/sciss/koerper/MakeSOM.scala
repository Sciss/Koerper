/*
 *  MakeSOM.scala
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

import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

import de.sciss.file.File
import de.sciss.kollflitz.Vec
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm.InMemory
import de.sciss.negatum.impl.SOMEval
import de.sciss.negatum.{Negatum, SOM}
import de.sciss.synth.proc.SoundProcesses

object MakeSOM {
  final val COOKIE = 0x57656967 // "Weig"

  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    type S = InMemory
    SoundProcesses.init()
    Negatum       .init()
    val system: S   = InMemory()
    val sCfg        = SOM.Config()

    val phases      = CreateSoundPool.mkFeatIterator().take(Prototype.NumSounds).map(_._1).toVector
    val numPhases   = phases.size
    val weights: Vec[Vec[Double]] = phases.map(readWeightVec)
    val uniqueWeights = weights.toSet.size

//    import de.sciss.numbers.Implicits._
    val numRepeats  = 3*3*3
    val numIter     = (numPhases * numRepeats * 1.5).toInt
    println(s"numIter = $numIter; numPhases = $numPhases; uniqueWeights = $uniqueWeights")

    sCfg.dimensions     = 2
    sCfg.extent         = 512
    sCfg.features       = 48
    sCfg.gridStep       = 4
    sCfg.learningCoef   = 0.072
    sCfg.numIterations  = numIter
//    sCfg.seed           = ...

    val _som = system.step { implicit tx =>
      val som = SOM[S](sCfg)
      val objects = Vector.tabulate(numPhases)(IntObj.newConst[S])
      for (ri <- 0 until numRepeats) {
        println(s"Repeat: $ri")
        weights.iterator.zipWithIndex.foreach { case (w, i) =>
          val o = objects(i)
          som.add(w, o)
//          println(s"  $i")
        }
        println(s"Now tree size is ${som.tree.size}")
      }

      var numItems = 0
      do {
        val itemSet = som.tree.iterator.map(_.value).toSet
        numItems = itemSet.size
        println(s"Number of unique items: $numItems")
        if (numItems < uniqueWeights) {
          println("Retry adding missing items...")
          weights.iterator.zipWithIndex.foreach { case (w, i) =>
            val o = objects(i)
            if (!itemSet.contains(o)) {
              som.add(w, o)
            }
          }
        }
      } while (numItems < uniqueWeights)

      println("Done.")
      som
    }

    system.step { implicit tx =>
      _som.tree.iterator.foreach { node =>
        println(s"${node.key} -> ${node.value},")
      }
    }
  }

  def writeWeight(fOut: File, weight: SOMEval.Weight): Unit = {
    val fos = new FileOutputStream(fOut)
    try {
      val dos = new DataOutputStream(fos)
      dos.writeInt(COOKIE)
      dos.writeShort(weight.spectral.length + weight.temporal.length)
      weight.spectral.foreach(dos.writeDouble)
      weight.temporal.foreach(dos.writeDouble)
    } finally {
      fos.close()
    }
  }

  def readWeightVec(fIn: File): Vec[Double] = {
    val fis = new FileInputStream(fIn)
    try {
      val dis = new DataInputStream(fis)
      val cookie = dis.readInt()
      require (cookie == COOKIE, s"Unexpected cookie ${cookie.toHexString} should be ${COOKIE.toHexString}")
      val vecSize = dis.readShort()
      Vector.fill(vecSize)(dis.readDouble())
    } finally {
      fis.close()
    }
  }

  def mkWeights(): Unit = {
    CreateSoundPool.mkPhaseIterator().foreach { case (fIn, idx) =>
      val fOut  = CreateSoundPool.formatTemplate(CreateSoundPool.tempFeat, idx + 1)
      if (fOut.length() == 0L) {
        val weightOpt: Option[SOMEval.Weight] = SOMEval(f = fIn)
        val weight = weightOpt.get // Await.result(weightF, Duration.Inf)
        writeWeight(fOut, weight)
        println(s"Done ${idx + 1}.")
      }
    }
  }
}
