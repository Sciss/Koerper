package de.sciss.koerper.lucre
package impl

import java.awt.{BorderLayout, EventQueue}
import java.net.InetSocketAddress
import java.util

import de.sciss.icons.raphael
import de.sciss.koerper.Koerper
import de.sciss.lucre.expr.{DoubleObj, IntObj, StringObj}
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.{stm, event => evt}
import de.sciss.mellite.gui.GUI
import de.sciss.neuralgas.sphere.SphereGNG.Config
import de.sciss.neuralgas.sphere.{Edge, Loc, LocVar, Node, Observer, PD, Polar}
import de.sciss.osc
import de.sciss.synth.proc.Workspace
import javax.swing.JPanel
import org.jzy3d.chart.{AWTChart, ChartLauncher}
import org.jzy3d.colors.Color
import org.jzy3d.maths.{Coord3d, Scale}
import org.jzy3d.plot3d.primitives.{LineStrip, Point}

import scala.collection.mutable
import scala.math.{cos, sin}
import scala.swing.event.ButtonClicked
import scala.swing.{Component, Dimension, ToggleButton}
import scala.util.control.NonFatal

object SphereGNGViewImpl {
  def apply[S <: Sys[S]](obj: SphereGNG[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                            workspace: Workspace[S]): SphereGNGView[S] = {
    new Impl[S].init(obj)
  }

  private final case class OscConfig(targetHost: String = Koerper.IpDavid, targetPort: Int = Koerper.OscPortDavid, 
                                     localHost: String = "", localPort: Int = 0,
                                     transport: osc.Transport = osc.UDP)

  private final class Impl[S <: Sys[S]](implicit val cursor: stm.Cursor[S], val workspace: Workspace[S])
    extends SphereGNGView[S] with ComponentHolder[Component] {

    private[this] var observer: Disposable[S#Tx] = _
    private[this] val algorithm = new SphereGNGImpl

    private[this] var gngCfg: Config    = _
    private[this] var oscCfg: OscConfig = _
    private[this] var chart : AWTChart  = _

    private[this] val defaultGngCfg = Config()
    private[this] val defaultOscCfg = OscConfig()

    private def mGngkConfig(map: evt.Map[S, String, Obj])(implicit tx: S#Tx): Unit = {
      def getDouble(key: String, default: Double): Double = map.get(key) match {
        case Some(v: DoubleObj[S])  => v.value
        case Some(v: IntObj   [S])  => v.value.toDouble
        case _                      => default
      }

      def getInt(key: String, default: Int): Int =  map.get(key) match {
        case Some(v: IntObj   [S])  => v.value
        case Some(v: DoubleObj[S])  => v.value.toInt
        case _                      => default
      }

      import SphereGNG._
      val c = Config(
        epsilon       = getDouble(attrGngEpsilon      , defaultGngCfg.epsilon     ),
        epsilon2      = getDouble(attrGngEpsilon2     , defaultGngCfg.epsilon2    ),
        beta          = getDouble(attrGngBeta         , defaultGngCfg.beta        ),
        alpha         = getDouble(attrGngAlpha        , defaultGngCfg.alpha       ),
        lambda        = getDouble(attrGngLambda       , defaultGngCfg.lambda      ),
        utility       = getDouble(attrGngUtility      , defaultGngCfg.utility     ),
        maxNodes0     = getInt   (attrGngMaxNodes     , defaultGngCfg.maxNodes0   ),
        maxEdgeAge    = getInt   (attrGngMaxEdgeAge   , defaultGngCfg.maxEdgeAge  ),
        maxNeighbors  = getInt   (attrGngMaxNeighbors , defaultGngCfg.maxNeighbors)
      )

      gngCfg = c
    }

    private def clearChart(): Unit = {
      chart.clear()
      chart.add(new Point(new Coord3d(-1, -1, -1), Color.WHITE, 0f))
      chart.add(new Point(new Coord3d(+1, +1, +1), Color.WHITE, 0f))
      setChartScale()
    }

    private def setChartScale(): Unit = {
      val scaleN = new Scale(-1, +1)
      val view = chart.getView
      view.setScaleX(scaleN)
      view.setScaleY(scaleN)
      view.setScaleZ(scaleN)
    }

    private def mkOscConfig(map: evt.Map[S, String, Obj])(implicit tx: S#Tx): Unit = {
      def getInt(key: String, default: Int): Int = map.get(key) match {
        case Some(v: IntObj[S]) => v.value
        case _                  => default
      }

      def getString(key: String, default: String): String = map.get(key) match {
        case Some(v: StringObj[S])  => v.value
        case _                      => default
      }

      import SphereGNG._
      val c = OscConfig(
        targetHost    = getString (attrOscTargetHost, defaultOscCfg.targetHost),
        targetPort    = getInt    (attrOscTargetPort, defaultOscCfg.targetPort),
        localHost     = getString (attrOscLocalHost , defaultOscCfg.localHost ),
        localPort     = getInt    (attrOscLocalPort , defaultOscCfg.localPort ),
        transport = osc.Transport(getString(attrOscTransport, defaultOscCfg.transport.name))
      )

      oscCfg = c
    }

    private def checkUpdate(map: evt.Map[S, String, Obj], key: String)(implicit tx: S#Tx): Unit = {
      if      (SphereGNG.configAttr.contains(key)) mGngkConfig (map)
      else if (SphereGNG.oscAttr   .contains(key)) mkOscConfig (map)
    }

    def init(obj: SphereGNG[S])(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      observer = obj.attr.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Obj.AttrAdded    (key, _)    => checkUpdate(upd.map, key)
          case Obj.AttrRemoved  (key, _)    => checkUpdate(upd.map, key)
          case Obj.AttrReplaced (key, _, _) => checkUpdate(upd.map, key)
          case _ =>
        }
      }
      val a = obj.attr
      mGngkConfig(a)
      mkOscConfig(a)
      this
    }

    private def guiInit(): Unit = {
      val ggPower = new ToggleButton {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            val sel = selected
//            SoundProcesses.atomic[S, Unit] { implicit tx =>
//              transport.stop()
//              if (added.swap(false)(tx.peer)) objH.foreach(h => transport.removeObject(h()))
//              transport.seek(0L)
//              if (sel) {
//                objH.foreach { h =>
//                  transport.addObject(h())
//                  added.set(true)(tx.peer)
//                }
//                transport.play()
//              }
//            } (transport.scheduler.cursor)
        }
      }
      val shpPower          = raphael.Shapes.Power _
      ggPower.icon          = GUI.iconNormal  (shpPower)
      ggPower.disabledIcon  = GUI.iconDisabled(shpPower)
      ggPower.tooltip       = "Run/Pause Process"

      chart = new AWTChart()
      clearChart()

      /* val mouse = */ ChartLauncher.configureControllers(chart, "SphereGNG", true, false)
      chart.render()
//      ChartLauncher.frame(chart, bounds, title)

      val p = new JPanel(new BorderLayout())
      val chartC = chart.getCanvas.asInstanceOf[java.awt.Component]
      chartC.setPreferredSize(new Dimension(480, 480))
      p.add(BorderLayout.CENTER , chartC)
      p.add(BorderLayout.SOUTH  , ggPower.peer)

      component = Component.wrap(p)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
    }

    // ---- PD ----

    private final class ProbDist(seed: Long, tableData: Array[Float],
                                 tableTheta: Array[Float], tablePhi: Array[Float]) extends PD {
      private[this] val rnd = new util.Random(seed)

      def poll(loc: LocVar): Unit = {
        val i0    = util.Arrays.binarySearch(tableData, 0, tableData.length, rnd.nextFloat())
        val i1    = if (i0 >= 0) i0 else -(i0 - 1)
        val dot   = if (i1 < tableData.length) i1 else tableData.length - 1
        val theta = tableTheta(dot)
        val phi   = tablePhi  (dot)
        if (theta == 0.0 && phi == 0.0) {
//          poll(loc)
          PD.Uniform.poll(loc)
        }
        else {
          loc.theta = theta
          loc.phi   = phi
        }
      }
    }

    // ---- sphere.Observer ----

    private final class oscObserver(oscT: osc.Transmitter.Undirected.Net, target: InetSocketAddress)
      extends Observer {

      private def send(m: osc.Message): Unit = try {
        oscT.send(m, target)
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

    private final class chartObserver(chart: AWTChart) extends Observer {
      var redraw: Boolean = true

      private[this] val nodeMap = mutable.Map.empty[Int , Point     ]
      private[this] val edgeMap = mutable.Map.empty[Long, LineStrip ]

      private def ensureEDT(body: => Unit): Unit =
        if (EventQueue.isDispatchThread) body else EventQueue.invokeLater(() => body)

      def clear(): Unit = ensureEDT {
//        chart.clear()
//        chart.add(new Point(new Coord3d(-1, -1, -1), Color.WHITE, 0f))
//        chart.add(new Point(new Coord3d(+1, +1, +1), Color.WHITE, 0f))
        nodeMap.clear()
        edgeMap.clear()
      }

      private def mkCoord(in: Loc): Coord3d = {
        import in._
        val sinTheta  = sin(theta)
        val x         = sinTheta * cos(phi)
        val y         = sinTheta * sin(phi)
        val z         = cos(theta)
        new Coord3d(x, y, z)
      }

      private def mkLineStrip(p1: Node, p2: Node): LineStrip = {
        val numIntp = math.max(2, (Polar.centralAngle(p1, p2) * 18).toInt)
        val c = Vector.tabulate(numIntp) { i =>
          val f = i.toDouble / (numIntp - 1)
          val p = Polar.interpolate(p1, p2, f)
          mkCoord(p)
        }

        val ln = new LineStrip(c: _*)
        ln.setWireframeColor(Color.BLACK)
        ln
      }

      def gngNodeUpdated(n: Node): Unit = ensureEDT {
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
        // log(s"gngNodeUpdated $ptOld -> $ptNew")
      }

      def gngNodeInserted(n: Node): Unit = ensureEDT {
        val ptNew = new Point(mkCoord(n), Color.RED, 3f)
        nodeMap(n.id) = ptNew
        chart.add(ptNew, redraw)
        // log(s"gngNodeInserted $ptNew")
      }

      def gngNodeRemoved(n: Node): Unit = ensureEDT {
        val ptOld = nodeMap.remove(n.id).get
        chart.removeDrawable(ptOld, redraw)
        // log(s"gngNodeRemoved $ptOld")
      }

      private def edgeId(from: Node, to: Node): Long = {
        val a = from.id
        val b = to  .id
        if (a < b) (a.toLong << 32) | (b.toLong & 0xFFFFFFFFL)
        else       (b.toLong << 32) | (a.toLong & 0xFFFFFFFFL)
      }

      // we do not visualise 'age'
      def gngEdgeUpdated(e: Edge): Unit = ()

      def gngEdgeInserted(e: Edge): Unit = ensureEDT {
        insertEdge(e.from, e.to, redraw = redraw)
        // log(s"gngEdgeInserted(${e.from.id}, ${e.to.id})")
      }

      def gngEdgeRemoved(e: Edge): Unit = ensureEDT {
        removeEdge(e.from, e.to, redraw = redraw)
        // log(s"gngEdgeRemoved(${e.from.id}, ${e.to.id})")
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

    private final class multiObserver(a: Observer, b: Observer) extends Observer {
      def gngNodeUpdated  (n: Node): Unit = { a.gngNodeUpdated  (n);  b.gngNodeUpdated  (n) }
      def gngEdgeUpdated  (e: Edge): Unit = { a.gngEdgeUpdated  (e);  b.gngEdgeUpdated  (e) }
      def gngNodeInserted (n: Node): Unit = { a.gngNodeInserted (n);  b.gngNodeInserted (n) }
      def gngEdgeInserted (e: Edge): Unit = { a.gngEdgeInserted (e);  b.gngEdgeInserted (e) }
      def gngNodeRemoved  (n: Node): Unit = { a.gngNodeRemoved  (n);  b.gngNodeRemoved  (n) }
      def gngEdgeRemoved  (e: Edge): Unit = { a.gngEdgeRemoved  (e);  b.gngEdgeRemoved  (e) }
    }
  }
}
