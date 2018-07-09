/*
 *  AnalyzeOsc.scala
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

import java.awt.geom.Point2D
import java.io.RandomAccessFile
import java.nio.ByteBuffer

import de.sciss.file._
import de.sciss.koerper.Prototype.{MaxNumTraj, Traj}
import de.sciss.kollflitz.Vec
import de.sciss.osc

object AnalyzeOsc {
  def main(args: Array[String]): Unit = {
    run()
  }

  final case class TimedMessage(time: Long, m: osc.Message)

  def run(): Unit = {
    val stream = readOsc()
    val dur = if (stream.size < 2) 0.0 else {
      (stream.last.time - stream.head.time) * 0.001
    }
    println(s"# messages: ${stream.size}; duration: $dur sec.")

    val a = analyze(stream)
    println(a)
  }

  case class AnalysisResult(trajLenCount      : Map[Int, Int], numTrajCount   : Map[Int, Int],
                            numIndivIsecCount : Map[Int, Int], numAllIsecCount: Map[Int, Int],
                            trajOffIsecCount  : Map[Int, Int], dimIsecCount   : Map[Int, Int]) {
    override def toString: String = {
      val s1 = s"$productPrefix(\n  trajLenCount = $trajLenCount,\n  numTrajCount = $numTrajCount,"
      val s2 = s"\n  numIndivIsecCount = $numIndivIsecCount,\n  numAllIsecCount = $numAllIsecCount,"
      val s3 = s"\n  trajOffIsecCount = $trajOffIsecCount,\n  dimIsecCount = $dimIsecCount\n)"
      s"$s1$s2$s3"
    }
  }

  sealed trait NoIntersection
  case object Separated   extends NoIntersection
  case object Coincident  extends NoIntersection
  case object Parallel    extends NoIntersection

  def intersectLineLine(a1x: Double, a1y: Double, a2x: Double, a2y: Double,
                        b1x: Double, b1y: Double, b2x: Double, b2y: Double): Either[NoIntersection, Point2D]= {
    val ua_t = (b2x - b1x) * (a1y - b1y) - (b2y - b1y) * (a1x - b1x)
    val ub_t = (a2x - a1x) * (a1y - b1y) - (a2y - a1y) * (a1x - b1x)
    val u_b  = (b2y - b1y) * (a2x - a1x) - (b2x - b1x) * (a2y - a1y)
    if (u_b != 0) {
      val ua = ua_t / u_b
      val ub = ub_t / u_b
      if (0 <= ua && ua <= 1 && 0 <= ub && ub <= 1) {
        val pt = new Point2D.Double(a1x + ua * (a2x - a1x), a1y + ua * (a2y - a1y))
        Right(pt)
      }
      else Left(Separated)
    }
    else Left(if (ua_t == 0 || ub_t == 0) Coincident else Parallel)
  }

  def analyze(stream: Vec[TimedMessage]): AnalysisResult = {
    var frameBuilder  = Map.empty[Int, Traj]  // id to traj
    var cyclicIds     = Set(0 until MaxNumTraj: _*)
    var idMap         = Map.empty[Int, Int]   // id to cId

    var trajLenCount      = Map.empty[Int, Int].withDefaultValue(0)   // traj-len to count
    var numTrajCount      = Map.empty[Int, Int].withDefaultValue(0)   // num-traj to count
    var numIndivIsecCount = Map.empty[Int, Int].withDefaultValue(0)   // num to count
    var numAllIsecCount   = Map.empty[Int, Int].withDefaultValue(0)   // num to count
    var trajOffIsecCount  = Map.empty[Int, Int].withDefaultValue(0)   // traj-offset to intersection count
    var dimIsecCount      = Map.empty[Int, Int].withDefaultValue(0)   // dim-index to ntersection count

    stream.foreach { case TimedMessage(_ /* time */, p) =>
      p match {
        case osc.Message("/f_new") =>
          val currentFrame = frameBuilder
          val numTraj = currentFrame.size
          numTrajCount += numTraj -> (numTrajCount(numTraj) + 1)
//          currentFrame.foreach { case (_, traj) =>
//            val trajLen = traj.pt.size
//            trajLenCount += trajLen -> (trajLenCount(trajLen) + 1)
//          }

          val numAllIsec: Int = currentFrame.valuesIterator.toList.combinations(2).map {
            case t1 :: t2 :: Nil if t1.pt.size >= 2 && t2.pt.size >= 2 =>
              val _ :+ p1 :+ q1 = t1.pt
              val _ :+ p2 :+ q2 = t2.pt
              val numIsec: Int = p1.indices.combinations(2).count {
                case Seq(dxi, dyi) =>
                  val p1x = p1(dxi)
                  val p1y = p1(dyi)
                  val q1x = q1(dxi)
                  val q1y = q1(dyi)
                  val p2x = p2(dxi)
                  val p2y = p2(dyi)
                  val q2x = q2(dxi)
                  val q2y = q2(dyi)
                  val res = intersectLineLine(
                    a1x = p1x, a1y = p1y, a2x = q1x, a2y = q1y,
                    b1x = p2x, b1y = p2y, b2x = q2x, b2y = q2y)
                  val intersects = res.isRight
                  if (intersects) {
                    val c1 = t1.pt.size - 2
                    val c2 = t2.pt.size - 2
                    trajOffIsecCount += c1  -> (trajOffIsecCount(c1 ) + 1)
                    trajOffIsecCount += c2  -> (trajOffIsecCount(c2 ) + 1)
                    dimIsecCount     += dxi -> (dimIsecCount    (dxi) + 1)
                    dimIsecCount     += dyi -> (dimIsecCount    (dyi) + 1)
                  }
                  intersects
              }
//              if (numIsec > 0) {
                numIndivIsecCount += numIsec -> (numIndivIsecCount(numIsec) + 1)
//              }

              numIsec

            case _ => 0

          } .sum

          numAllIsecCount += numAllIsec -> (numAllIsecCount(numAllIsec) + 1)

        case osc.Message("/t_set", id: Int,
        c0: Float, c1: Float, c2: Float, c3: Float, c4: Float, c5: Float /*, c6: Float, c7: Float */) =>
          require(Prototype.Dimensions == 6)
          val t0Opt  = frameBuilder.get(id)
          t0Opt.foreach { t0 =>
            val t1 = t0 // if (t0.pt.size < VisualTrajLen) t0 else t0.copy(pt = t0.pt.tail)
            val c = {
              val a = new Array[Float](Prototype.Dimensions)
              a(0) = c0
              a(1) = c1
              a(2) = c2
              a(3) = c3
              a(4) = c4
              a(5) = c5
//              a(6) = c6
//              a(7) = c7
              a
            }
            val t2 = t1.copy(pt = t1.pt :+ c)
            frameBuilder += id -> t2
          }

        case osc.Message("/t_new", id: Int) =>
          cyclicIds.headOption.foreach { cId =>
            cyclicIds -= cId
            idMap.get(id).foreach { cId => cyclicIds += cId }
            idMap += id -> cId
            val t = Traj(cId = cId, pt = Vector.empty)
            frameBuilder += id -> t
          }

        case osc.Message("/t_end", id: Int) =>
          frameBuilder.get(id).foreach { traj =>
            val trajLen = traj.pt.size
            trajLenCount += trajLen -> (trajLenCount(trajLen) + 1)
          }
          frameBuilder -= id
          idMap.get(id).foreach { cId =>
            cyclicIds += cId
            idMap -= id
          }

        case _ =>
          println(s"Warning: dropping unknown OSC packet $p")
      }
    }

    // remove initial f_new effect
    numTrajCount.get(0).foreach { c0 =>
      val c1 = c0 - 1
      numTrajCount = if (c1 == 0) numTrajCount - 0 else numTrajCount + (0 -> c1)
    }

    AnalysisResult(trajLenCount = trajLenCount, numTrajCount = numTrajCount,
      numIndivIsecCount = numIndivIsecCount, numAllIsecCount = numAllIsecCount,
      trajOffIsecCount = trajOffIsecCount, dimIsecCount = dimIsecCount)
  }

  def readOsc(): Vec[TimedMessage] = {
    val f   = file("/data/temp/beta-osc.bin")
    val b   = Vector.newBuilder[TimedMessage]
    val bb  = ByteBuffer.allocate(8192)
    val c   = osc.PacketCodec.default
    val raf = new RandomAccessFile(f, "r")
    try {
      val fch = raf.getChannel
      while (raf.getFilePointer < raf.length()) {
        val time  = raf.readLong()
        val sz    = raf.readInt()
        bb.clear().limit(sz)
        fch.read(bb)
        bb.flip()
        c.decode(bb) match {
          case m: osc.Message => b += TimedMessage(time, m)
          case _ =>
        }
      }
      b.result()

    } finally {
      raf.close()
    }
  }
}
