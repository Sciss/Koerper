/*
 *  Neural.scala
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

import java.util

import de.sciss.koerper.Raster.{RasterSize, mkSphereCoordinatesTable, mkStochasticTable}
import de.sciss.neuralgas.sphere.{LocVar, PD, Polar, SphereGNG}
import org.jzy3d.chart.{AWTChart, ChartLauncher}
import org.jzy3d.colors.Color
import org.jzy3d.maths.{Coord3d, Scale}
import org.jzy3d.plot3d.primitives.{LineStrip, Point}
import org.jzy3d.plot3d.rendering.canvas.Quality

import scala.annotation.tailrec
import scala.math.{cos, sin}

object Neural {
  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
    val tableCoord  = mkSphereCoordinatesTable()
    val tableData   = mkStochasticTable()
    val N           = RasterSize / 15; // sqrt(RasterSize).toInt

    final class TestPD(seed: Long) extends PD {
      private[this] val rnd = new util.Random(seed)

      @tailrec
      def poll(loc: LocVar): Unit = {
        val i0    = util.Arrays.binarySearch(tableData, rnd.nextFloat())
        val i1    = if (i0 >= 0) i0 else -(i0 - 1)
        val dot   = if (i1 < tableData.length) i1 else tableData.length - 1
        val dotL  = dot << 1
        val theta = tableCoord(dotL)
        val phi   = tableCoord(dotL + 1)
        if (theta == 0.0 && phi == 0.0) poll(loc)
        else {
          loc.theta = theta
          loc.phi   = phi
        }
      }
    }

    val config = SphereGNG.Config(
      pd          = new TestPD(seed = 12345),
      maxEdgeAge  = 5000,
      utility     = 20,
      beta        = 0.0005,
      epsilon     = 0.1,
      epsilon2    = 0.001,
      alpha       = 0.5,
      lambda      = 1.0/50,
      maxNodes0   = N
    )
    val sphere = SphereGNG(config)

//    sphere.step()
    for (_ <- 0 until 40000) sphere.step()

    val chart = new AWTChart(Quality.Nicest)
    val sq = sphere.nodeIterator.toList
    println(s"N = $N, sq.size = ${sq.size}")

    def mkCoord(in: Polar): Coord3d = {
      import in._
      val sinTheta  = sin(theta)
      val x         = sinTheta * cos(phi)
      val y         = sinTheta * sin(phi)
      val z         = cos(theta)
      new Coord3d(x, y, z)
    }

    sphere.edgeIterator.foreach { case (p1, p2) =>
      val numIntp = math.max(2, (Polar.centralAngle(p1, p2) * 20).toInt)
      val c = Vector.tabulate(numIntp) { i =>
        val f = i.toDouble / (numIntp - 1)
        val p = Polar.interpolate(p1, p2, f)
        mkCoord(p)
      }

      val ln = new LineStrip(c: _*)
      ln.setWireframeColor(Color.BLACK)
      chart.add(ln)
    }


    sq.foreach { p =>
      println(p)
      if (!p.phi.isNaN && !p.theta.isNaN) {
        val c = mkCoord(p)
        chart.add(new Point(c, Color.RED, 5f))
      }
    }

    val view = chart.getView
    view.setScaleX(new Scale(-1, +1))
    view.setScaleY(new Scale(-1, +1))
    view.setScaleZ(new Scale(-1, +1))

    ChartLauncher.openChart(chart)
  }
}
