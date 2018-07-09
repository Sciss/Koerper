/*
 *  AdHocMap.scala
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

import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.IntSpace.TwoDim
import de.sciss.lucre.geom.{IntDistanceMeasure2D, IntPoint2D, IntSquare}
import de.sciss.lucre.stm.DummySerializerFactory
import de.sciss.lucre.synth.InMemory

import scala.annotation.tailrec

object AdHocMap {
  def main(args: Array[String]): Unit = {
//    val t0 = System.currentTimeMillis()
    run()
//    val t1 = System.currentTimeMillis()
//    println(s"Took ${t1-t0} ms")
  }

  case class Key(pt: IntPoint2D, id: Int)

  def mkInputIterator(): Iterator[Key] = {
    val url = this.getClass.getResource("/Map2D.txt")
    io.Source.fromURL(url, "UTF-8").getLines().map { ln =>
      val arr = ln.split(",")
      val x   = arr(0).toInt
      val y   = arr(1).toInt
      val id  = arr(2).toInt
      Key(IntPoint2D(x, y), id = id)
    }
  }

  def run(): Unit = {
    type S = InMemory
    val system: S = InMemory()
    type D = TwoDim

    implicit val ptView: (Key, S#Tx) => D#PointLike = (key, _) => key.pt

    val dummy = DummySerializerFactory[S]
    import dummy.dummySerializer

    system.step { implicit tx =>
      val map = SkipOctree.empty[S, D, Key](IntSquare(512, 512, 512))
      mkInputIterator().foreach(map.add)
      val mDownRight = IntDistanceMeasure2D.euclideanSq.orthant(3)

      @tailrec
      def loop(pt: IntPoint2D, res: Int): Int = {
        map.nearestNeighborOption(pt, mDownRight) match {
          case Some(key) =>
            println(key)
            loop(IntPoint2D(key.pt.x + 1, key.pt.y + 1), res + 1)

          case None => res
        }
      }

      val num = loop(IntPoint2D(0, 0), 0)
      println(s"Number of points traversed: $num")
    }
  }
}
