/*
 *  ProbDist.scala
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

package de.sciss.koerper.lucre
package impl

import java.util

import de.sciss.file.File
import de.sciss.lucre.stm.TxnLike
import de.sciss.neuralgas.sphere.{LocVar, PD}
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.SoundProcesses

object ProbDist {
  def read(f: File)(implicit tx: TxnLike): Processor[ProbDist] = {
    val p = new ReadImpl(f)
    import SoundProcesses.executionContext
    tx.afterCommit(p.start())
    p
  }

  private final class ReadImpl(f: File)
    extends ProcessorImpl[ProbDist, Processor[ProbDist]] with Processor[ProbDist] {

    protected def body(): ProbDist = {
      val af = AudioFile.openRead(f)
      try {
        val sz0   = af.numFrames.toInt
        val sz    = math.max(1, sz0)
        val buf   = af.buffer(sz)
        var off   = 0
        while (off < sz0) {
          checkAborted()
          val chunk = math.min(sz0 - off, 8192)
          af.read(buf, off, chunk)
          off += chunk
        }

        val tableData   = buf(0)
        val tableTheta  = buf(1)
        val tablePhi    = buf(2)

        // integrate table -- this has been removed from FSc now
        var i = 0
        var sum = 0.0
        while (i < sz0) {
          val value = tableData(i)
          sum += value
          tableData(i) = sum.toFloat
          i += 1
        }

        val pd = new ProbDistImpl(seed = 0L, tableData = tableData, tableTheta = tableTheta, tablePhi = tablePhi)
        pd

      } finally {
        af.close()
      }
    }
  }

  private final class ProbDistImpl(seed: Long, tableData: Array[Float],
                       tableTheta: Array[Float], tablePhi: Array[Float]) extends ProbDist {
    private[this] val rnd = new util.Random(seed)
    private[this] val sz  = tableData.length
    private[this] val _energy = tableData(sz - 1)

    private[this] val max = if (_energy > 0f) _energy else 0.1f

    def energy: Double = _energy

    def poll(loc: LocVar): Unit = {
      val i0    = util.Arrays.binarySearch(tableData, 0, sz, rnd.nextFloat() * max)
      val i1    = if (i0 >= 0) i0 else -(i0 - 1)
      val dot   = if (i1 < tableData.length) i1 else sz - 1
      val theta = tableTheta(dot)
      val phi   = tablePhi  (dot)
      if (theta == 0.0 && phi == 0.0) {
        PD.Uniform.poll(loc)
      }
      else {
        loc.theta = theta
        loc.phi   = phi
      }
    }
  }
}
trait ProbDist extends PD {
  def energy: Double
}