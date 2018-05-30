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

import de.sciss.koerper.Raster.{RasterSize, mkStochasticTable}
import de.sciss.neuralgas.sphere.{Edge, Loc, LocVar, Node, Observer, PD, Polar, SphereGNG}
import org.jzy3d.chart.{AWTChart, ChartLauncher}
import org.jzy3d.colors.Color
import org.jzy3d.maths.{Coord3d, Scale}
import org.jzy3d.plot3d.primitives.{LineStrip, Point}
import org.jzy3d.plot3d.rendering.canvas.Quality
import org.jzy3d.plot3d.rendering.view.modes.ViewBoundMode

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.{cos, sin}
import scala.swing.{BoxPanel, Button, Frame, Label, Orientation}

object Neural {
  def main(args: Array[String]): Unit = {
    run()
  }

  def run(): Unit = {
//    val tableCoord  = mkSphereCoordinatesTable()
    val (tableCoord, tableData, tableSize) = mkStochasticTable()
    val N           = RasterSize / 15; // sqrt(RasterSize).toInt

    final class TestPD(seed: Long) extends PD {
      private[this] val rnd = new util.Random(seed)

      @tailrec
      def poll(loc: LocVar): Unit = {
        val i0    = util.Arrays.binarySearch(tableData, 0, tableSize, rnd.nextFloat())
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

    val chart = new AWTChart(Quality.Nicest)
//    val sq = sphere.nodeIterator.toList
//    println(s"N = $N, sq.size = ${sq.size}")

    def mkCoord(in: Loc): Coord3d = {
      import in._
      val sinTheta  = sin(theta)
      val x         = sinTheta * cos(phi)
      val y         = sinTheta * sin(phi)
      val z         = cos(theta)
      new Coord3d(x, y, z)
    }

//    val loc = new LocVar
//    val sq = Iterator.fill(100000) { config.pd.poll(loc); Polar(loc.theta, loc.phi) }
//    sq.foreach { p =>
//      if (!p.phi.isNaN && !p.theta.isNaN) {
//        val c = mkCoord(p)
//        chart.add(new Point(c, Color.BLACK, 2f))
//      }
//    }

//    sphere.edgeIterator.foreach { e =>
//      val p1 = e.from.toPolar
//      val p2 = e.to  .toPolar
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
//      chart.add(ln)
//    }

//    sq.foreach { p =>
////      println(p)
//      if (!p.phi.isNaN && !p.theta.isNaN) {
//        val c = mkCoord(p)
//        chart.add(new Point(c, Color.RED, 5f))
//      } else {
//        println("NaN!")
//      }
//    }

    val scaleN = new Scale(-1, +1)

    def setScale(): Unit = {
      val view = chart.getView
      view.setScaleX(scaleN)
      view.setScaleY(scaleN)
      view.setScaleZ(scaleN)
    }

    def log(what: => String): Unit = ()  // println(what)

    def mkLineStrip(p1: Node, p2: Node): LineStrip = {
//      val p1 = e.from
//      val p2 = e.to

      val numIntp = math.max(2, (Polar.centralAngle(p1, p2) * 20).toInt)
      val c = Vector.tabulate(numIntp) { i =>
        val f = i.toDouble / (numIntp - 1)
        val p = Polar.interpolate(p1, p2, f)
        mkCoord(p)
      }

      val ln = new LineStrip(c: _*)
      ln.setWireframeColor(Color.BLACK)
      ln
    }

    object obs extends Observer {
      var redraw: Boolean = true

      private[this] val nodeMap = mutable.Map.empty[Int , Point     ]
      private[this] val edgeMap = mutable.Map.empty[Long, LineStrip ]

      def gngNodeUpdated(n: Node): Unit = {
        val ptOld = nodeMap(n.id)
        chart.removeDrawable(ptOld, false)
        val ptNew = new Point(mkCoord(n), Color.RED, 3f)
        nodeMap(n.id) = ptNew
        for (ni <- 0 until n.numNeighbors) {
          val nb = n.neighbor(ni)
          removeEdge(n, nb, redraw = false)
          insertEdge(n, nb, redraw = false)
        }
        chart.add(ptNew, redraw)
//        setScale()
        log(s"gngNodeUpdated $ptOld -> $ptNew")
      }

      def gngNodeInserted(n: Node): Unit = {
        val ptNew = new Point(mkCoord(n), Color.RED, 3f)
        nodeMap(n.id) = ptNew
        chart.add(ptNew, redraw)
        log(s"gngNodeInserted $ptNew")
      }

      def gngNodeRemoved(n: Node): Unit = {
        val ptOld = nodeMap.remove(n.id).get
        chart.removeDrawable(ptOld, redraw)
        log(s"gngNodeRemoved $ptOld")
      }

      private def edgeId(from: Node, to: Node): Long = {
        val a = from.id
        val b = to  .id
        if (a < b) (a.toLong << 32) | (b.toLong & 0xFFFFFFFFL)
        else       (b.toLong << 32) | (a.toLong & 0xFFFFFFFFL)
      }

      // we do not visualise 'age'
      def gngEdgeUpdated(e: Edge): Unit = ()

      def gngEdgeInserted(e: Edge): Unit = {
        insertEdge(e.from, e.to, redraw = redraw)
        log(s"gngEdgeInserted(${e.from.id}, ${e.to.id})")
      }

      def gngEdgeRemoved(e: Edge): Unit = {
        removeEdge(e.from, e.to, redraw = redraw)
        log(s"gngEdgeRemoved(${e.from.id}, ${e.to.id})")
      }

      private def insertEdge(from: Node, to: Node, redraw: Boolean): Unit = {
        val lnNew = mkLineStrip(from, to)
        val id = edgeId(from, to)
        edgeMap(id) = lnNew
        chart.add(lnNew, redraw)
      }

      private def removeEdge(from: Node, to: Node, redraw: Boolean): Unit = {
        val id = edgeId(from, to)
        val lnOld = edgeMap.remove(id).get
        chart.removeDrawable(lnOld, redraw)
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
      maxNodes0   = N,
      observer    = obs
    )

    val sphere = SphereGNG(config)

    setScale()
//    chart.getView.setBoundMode(ViewBoundMode.MANUAL)  // does not help

    // cheesy trick to avoid the auto-rescaling of the chart when removing points
    chart.add(new Point(new Coord3d(-1, -1, -1), Color.WHITE, 0f))
    chart.add(new Point(new Coord3d(+1, +1, +1), Color.WHITE, 0f))

    ChartLauncher.openChart(chart)

    var frameCount = 0

    val lb = new Label(frameCount.toString)

    new Frame {
      contents = new BoxPanel(Orientation.Vertical) {
        contents += Button("Step") {
          obs.redraw = false
          val numSteps = 50
          for (_ <- 1 until numSteps) sphere.step()
          obs.redraw = true
          sphere.step()
          frameCount += numSteps
          lb.text = frameCount.toString
        }
        contents += lb
      }

      pack().centerOnScreen()
      open()
    }
  }
}
