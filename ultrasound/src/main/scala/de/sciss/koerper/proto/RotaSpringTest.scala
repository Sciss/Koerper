/*
 *  RotaSpringTest.scala
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
package proto

import de.sciss.neuralgas.sphere.{Loc, Pt3}
import org.jzy3d.chart.{AWTChart, ChartLauncher}
import org.jzy3d.colors.Color
import org.jzy3d.maths.{Coord3d, Scale}
import org.jzy3d.plot3d.primitives.Point
import org.jzy3d.plot3d.rendering.canvas.Quality

import scala.math.{cos, sin}
import scala.swing.{BoxPanel, Button, Frame, Orientation}

object RotaSpringTest {
  def main(args: Array[String]): Unit = {
    test3dView()
  }

  def test3dView(): Unit = {
    val chart = new AWTChart(Quality.Nicest)

    def mkCoord(in: Loc): Coord3d = {
      import in._
      val sinTheta  = sin(theta)
      val x         = sinTheta * cos(phi)
      val y         = sinTheta * sin(phi)
      val z         = cos(theta)
      new Coord3d(x, y, z)
    }

    val scaleN = new Scale(-1, +1)

    def setScale(): Unit = {
      val view = chart.getView
      view.setScaleX(scaleN)
      view.setScaleY(scaleN)
      view.setScaleZ(scaleN)
    }

//    def mkLineStrip(p1: Node, p2: Node): LineStrip = {
//      //      val p1 = e.from
//      //      val p2 = e.to
//
//      val numIntp = math.max(2, (Polar.centralAngle(p1, p2) * 20).toInt)
//      val c = Vector.tabulate(numIntp) { i =>
//        val f = i.toDouble / (numIntp - 1)
//        val p = Polar.interpolate(p1, p2, f)
//        mkCoord(p)
//      }
//
//      val ln = new LineStrip(c: _*)
//      ln.setWireframeColor(Color.BLACK)
//      ln
//    }

    setScale()

    // cheesy trick to avoid the auto-rescaling of the chart when removing points
    chart.add(new Point(new Coord3d(-1, -1, -1), Color.WHITE, 0f))
    chart.add(new Point(new Coord3d(+1, +1, +1), Color.WHITE, 0f))

    ChartLauncher.openChart(chart)

    var attractor : Pt3   = null
    var attractorC: Point = null

    def attrJump(): Unit = {
      if (attractorC != null) {
        chart.removeDrawable(attractorC, false)
      }
      attractor   = Pt3(math.random(), math.random(), math.random()).normalized
      attractorC  = new Point(mkCoord(attractor.toPolar), Color.BLUE, 4f)
      chart.add(attractorC, true)
    }

    attrJump()

    var vec : Pt3   = Pt3(0, 1, 0).normalized
    var velo: Pt3   = Pt3(0, 0, 0.01)
    var vecC: Point = null

    def vecMove(): Unit = {
      if (vecC != null) {
        chart.removeDrawable(vecC, false)
      }
      vec   = (vec + velo).normalized
      vecC  = new Point(mkCoord(vec.toPolar), Color.RED, 4f)
      chart.add(vecC, true)
    }

    vecMove()

    val timer = new javax.swing.Timer((1000.0/25).toInt, { _ =>
      vecMove()
    })
    timer.setRepeats  (true)
    timer.setCoalesce (true)
    timer.restart()

    new Frame {
      contents = new BoxPanel(Orientation.Vertical) {
        contents += Button("Step") {
          attrJump()
        }
//        contents += lb
      }

      pack().centerOnScreen()
      open()
    }
  }
}
