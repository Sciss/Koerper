/*
 *  Neural.scala
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

import java.net.InetSocketAddress
import java.util

import de.sciss.koerper.proto.Raster.{RasterSize, mkStochasticTable}
import de.sciss.neuralgas.sphere.{Edge, Loc, LocVar, Node, Observer, PD, Polar, SphereGNG}
import de.sciss.osc
import org.jzy3d.chart.{AWTChart, ChartLauncher}
import org.jzy3d.colors.Color
import org.jzy3d.maths.{Coord3d, Scale}
import org.jzy3d.plot3d.primitives.{LineStrip, Point}
import org.jzy3d.plot3d.rendering.canvas.Quality

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.{cos, sin}
import scala.swing.{BoxPanel, Button, Frame, Label, Orientation}
import scala.util.control.NonFatal

object Neural {
  def main(args: Array[String]): Unit = {
    testSendOsc()
  }

  def testSendOsc(): Unit = {
    val N = 125 // RasterSize / 15; // sqrt(RasterSize).toInt

    val oscCfg    = osc.UDP.Config()
    oscCfg.codec  = osc.PacketCodec().doublePrecision()
    val target    = new InetSocketAddress(Koerper.IpDavid, Koerper.OscPortDavid)
    val oscT      = osc.UDP.Transmitter(oscCfg)
    oscT.connect()

    oscT.dump()

    object obs extends Observer {
      // private def log(what: => String): Unit = ()  // println(what)

      private var COUNT = 0

      private def send(m: osc.Message): Unit = try {
        COUNT += 1
//        if (COUNT <= 3) {
          oscT.send(m, target)
//        }
      } catch {
        case NonFatal(_) =>
      }

      def gngNodeInserted(n: Node): Unit = {
        // [ "/n_new", <(int32) id>, <(float64) theta>, <(float64) phi> ]
        this send osc.Message("/n_new", n.id: Int, n.theta: Double, n.phi: Double)
      }

      def gngNodeRemoved(n: Node): Unit = {
        // [ "/n_end", <(int32) id> ]
        this send osc.Message("/n_end", n.id: Int)
      }

      def gngNodeUpdated(n: Node): Unit = {
        // [ "/n_set", <(int32) id>, <(float64) theta>, <(float64) phi> ]
        this send osc.Message("/n_set", n.id: Int, n.theta: Double, n.phi: Double)
      }

      private def edgeChange(cmd: String, e: Edge): Unit = {
        val idFrom  = e.from.id
        val idTo    = e.to  .id
        val id1     = if (idFrom <= idTo) idFrom else idTo
        val id2     = if (idFrom <= idTo) idTo   else idFrom
        this send osc.Message(cmd, id1: Int, id2: Int)
      }

      def gngEdgeInserted(e: Edge): Unit = {
        // [ "/e_new", <(int32) node-id1>, <(int32) node-id2> ]
        edgeChange("/e_new", e)
      }

      def gngEdgeRemoved(e: Edge): Unit = {
        // [ "/e_end", <(int32) node-id1>, <(int32) node-id2> ]
        edgeChange("/e_end", e)
      }

      def gngEdgeUpdated(e: Edge): Unit = {
        // [ "/e_age", <(int32) node-id1>, <(int32) node-id2> ]
        edgeChange("/e_age", e)
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

    val sphere    = SphereGNG(config)
    val thread    = new Thread("osc-send") {
      override def run(): Unit = {
        while (true) {
          sphere.step()
          Thread.sleep(10)
        }
      }
    }

    thread.start()
  }

  final class TestPD(seed: Long) extends PD {
    private[this] val (tableCoord, tableData, tableSize) = mkStochasticTable()
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

  def test3dView(): Unit = {
//    val tableCoord  = mkSphereCoordinatesTable()
    val N           = RasterSize / 15; // sqrt(RasterSize).toInt

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
          val numSteps = 100
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
