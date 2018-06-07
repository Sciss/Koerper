/*
 *  SphereGNGViewImpl.scala
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

import java.awt.{BorderLayout, EventQueue}
import java.net.InetSocketAddress
import java.util.TimerTask
import java.util.concurrent.ConcurrentLinkedDeque

import de.sciss.icons.raphael
import de.sciss.koerper.Koerper
import de.sciss.lucre.expr.{DoubleObj, IntObj, StringObj}
import de.sciss.lucre.stm.{Disposable, Obj, Sys, TxnLike}
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.{stm, event => evt}
import de.sciss.mellite.gui.GUI
import de.sciss.numbers
import de.sciss.neuralgas.sphere.SphereGNG.Config
import de.sciss.neuralgas.sphere.{Edge, Loc, Node, Observer, PD}
import de.sciss.osc
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{AudioCue, Workspace}
import javax.swing.JPanel
import org.jzy3d.chart.{AWTChart, ChartLauncher}
import org.jzy3d.colors.Color
import org.jzy3d.maths.{Coord3d, Scale}
import org.jzy3d.plot3d.primitives.{LineStrip, Point}

import scala.collection.mutable
import scala.concurrent.stm.{InTxn, Ref}
import scala.swing.event.ButtonClicked
import scala.swing.{Button, Component, Dimension, FlowPanel, ToggleButton}
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
    extends SphereGNGView[S] with ComponentHolder[Component] { impl =>

    private[this] var observer: Disposable[S#Tx] = _
    private[this] val algorithm = new SphereGNGImpl

    @volatile
    @volatile
    private[this] var gngCfg  : Config    = _

    @volatile
    private[this] var oscCfg  : OscConfig = _

    @volatile
    private[this] var throttle: Double    = _

    private[this] var _chart  : AWTChart  = _

    private[this] val runChart  = Ref(true  )
    private[this] val runOsc    = Ref(false )

    private[this] val defaultGngCfg = Config()
    private[this] val defaultOscCfg = OscConfig()

    private[this] val pdRef       = Ref[PD](PD.Uniform)
    private[this] val oscRef      = Ref(Option.empty[OscObserver])
    private[this] val oscDumpRef  = Ref(false)

    private type EvtMap = evt.Map[S, String, Obj]

    private def getDouble(key: String, default: Double)(implicit tx: S#Tx, map: EvtMap): Double =
      map.get(key) match {
        case Some(v: DoubleObj[S])  => v.value
        case Some(v: IntObj   [S])  => v.value.toDouble
        case _                      => default
      }

    // accepts doubles as well
    private def getIntD(key: String, default: Int)(implicit tx: S#Tx, map: EvtMap): Int =
      map.get(key) match {
        case Some(v: IntObj   [S])  => v.value
        case Some(v: DoubleObj[S])  => v.value.toInt
        case _                      => default
      }

    private def getInt(key: String, default: Int)(implicit tx: S#Tx, map: EvtMap): Int =
      map.get(key) match {
        case Some(v: IntObj[S]) => v.value
        case _                  => default
      }

    private def getString(key: String, default: String)(implicit tx: S#Tx, map: EvtMap): String =
      map.get(key) match {
        case Some(v: StringObj[S])  => v.value
        case _                      => default
      }

    private def mkAndStoreOscObserver()(implicit tx: InTxn): OscObserver = {
      disposeOscObserver()
      val c       = oscCfg
      val codec   = osc.PacketCodec().doublePrecision().build
      val oscT    = if (c.transport == osc.UDP) {
        val c1        = osc.UDP.Config()
        c1.codec      = codec
        c1.localPort  = c.localPort
        if (!c.localHost.isEmpty) c1.localSocketAddress = new InetSocketAddress(c.localHost, c.localPort)
        osc.UDP.Transmitter(c1)
      } else {
        val c1        = osc.TCP.Config()
        c1.codec      = codec
        c1.localPort  = c.localPort
        if (!c.localHost.isEmpty) c1.localSocketAddress = new InetSocketAddress(c.localHost, c.localPort)
        ??? // osc.TCP.Transmitter(c1)
      }
      oscT.connect()
      if (oscDumpRef()) oscT.dump()
      val target  = new InetSocketAddress(c.targetHost, c.targetPort)
      val res     = new OscObserver(oscT, target)
      oscRef()    = Some(res)
      res
    }

    private def disposeOscObserver()(implicit tx: InTxn): Unit =
      oscRef.swap(None).foreach(_.oscT.close())

    private def dumpOsc(onOff: Boolean)(implicit tx: InTxn): Unit = {
      oscRef().foreach(_.oscT.dump(if (onOff) osc.Dump.Text else osc.Dump.Off))
      oscDumpRef() = onOff
    }

    private def mkObs()(implicit tx: InTxn): Observer = {
      val _chart  = runChart()
      val _osc    = runOsc  ()
      val obs     = if (_chart && _osc) {
        val obs1 = mkAndStoreOscObserver()
        val obs2 = new ChartObserver
        new MultiObserver(obs1, obs2)
      } else if (_chart) {
        new ChartObserver
      } else if (_osc) {
        mkAndStoreOscObserver()
      } else {
        Observer.Dummy
      }
      obs
    }

    private def mkGngConfig()(implicit tx: S#Tx, map: EvtMap): Unit = {
      import SphereGNG._
      import TxnLike.peer

      val _obs  = mkObs()
      val pd    = pdRef()

      val maxNodes = pd match {
        case _pd: ProbDist =>
          val energy        = _pd.energy
          val maxNodes0     = getIntD   (attrGngMaxNodes  , defaultGngCfg.maxNodes0)
          val minEnergy     = getDouble (attrGngMinEnergy , 0.0)
          val maxEnergy     = math.max(minEnergy + 1.0, getDouble (attrGngMaxEnergy, energy))
          import numbers.Implicits._
          energy.clip(minEnergy, maxEnergy).linLin(minEnergy, maxEnergy, 0.0, maxNodes0).round.toInt

        case _ =>
          getIntD   (attrGngMaxNodes  , defaultGngCfg.maxNodes0)
      }

      println(s"maxNodes = $maxNodes")

      val c = Config(
        epsilon       = getDouble(attrGngEpsilon      , defaultGngCfg.epsilon     ),
        epsilon2      = getDouble(attrGngEpsilon2     , defaultGngCfg.epsilon2    ),
        beta          = getDouble(attrGngBeta         , defaultGngCfg.beta        ),
        alpha         = getDouble(attrGngAlpha        , defaultGngCfg.alpha       ),
        lambda        = getDouble(attrGngLambda       , defaultGngCfg.lambda      ),
        utility       = getDouble(attrGngUtility      , defaultGngCfg.utility     ),
        maxNodes0     = maxNodes, // getIntD  (attrGngMaxNodes     , defaultGngCfg.maxNodes0   ),
        maxEdgeAge    = getIntD  (attrGngMaxEdgeAge   , defaultGngCfg.maxEdgeAge  ),
        maxNeighbors  = getIntD  (attrGngMaxNeighbors , defaultGngCfg.maxNeighbors),
        observer      = _obs,
        pd            = pd
      )

      gngCfg = c
    }

    private def updateObs()(implicit tx: InTxn): Unit = {
      val _obs  = mkObs()
      val c     = gngCfg.copy(observer = _obs)
      gngCfg = c
    }

    private def mkThrottle()(implicit tx: S#Tx, map: EvtMap): Unit = {
      throttle = getDouble(SphereGNG.attrGngThrottle, SphereGNG.DefaultThrottle)
    }

    private def clearChart(): Unit = {
      _chart.clear()
      _chart.add(new Point(new Coord3d(-1, -1, -1), Color.WHITE, 0f))
      _chart.add(new Point(new Coord3d(+1, +1, +1), Color.WHITE, 0f))
      setChartScale()
    }

    private def setChartScale(): Unit = {
      val scaleN = new Scale(-1, +1)
      val view = _chart.getView
      view.setScaleX(scaleN)
      view.setScaleY(scaleN)
      view.setScaleZ(scaleN)
    }

    private def mkOscConfig()(implicit tx: S#Tx, map: EvtMap): Unit = {
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

    private def checkUpdate(map: EvtMap, key: String)(implicit tx: S#Tx): Unit = {
      import TxnLike.peer
      implicit val _map: EvtMap = map
      if (SphereGNG.configAttr.contains(key)) {
        mkGngConfig()
      }
      else if (SphereGNG.oscAttr.contains(key)) {
        mkOscConfig()
        if (runOsc()) mkGngConfig()
      }
      else if (key == SphereGNG.attrTable) {
        if (updateCue()) {
          mkGngConfig()
        }
      }
      else if (key == SphereGNG.attrGngThrottle) {
        mkThrottle()
      }
    }

    private[this] val timerRef = Ref(Option.empty[java.util.Timer])

    private def isAlgorithmRunning: Boolean = timerRef.single.get.isDefined

    private def startAlgorithm(): Unit = {
      val t   = new java.util.Timer("run-gng")
      val dly = math.max(1, (1000 / math.max(1, throttle)).toInt)
      val tt  = new TimerTask {
        def run(): Unit = {
          algorithm.step()(gngCfg)
          oscRef.single.get.foreach { oscObs =>
            oscObs.send(osc.Message("/frame"))
          }
        }
      }
      t.scheduleAtFixedRate(tt, dly /* 0L */, dly)
      timerRef.single.swap(Some(t)).foreach(_.cancel())
    }

    private def stopAlgorithm(): Unit = {
      timerRef.single.swap(None).foreach(_.cancel())
    }

    private def updateCue()(implicit tx: S#Tx, map: EvtMap): Boolean = {
      import TxnLike.peer
      val opt = map.$[AudioCue.Obj](SphereGNG.attrTable)
      opt.foreach { cue =>
        val cueV  = cue.value
        val af    = AudioFile.openRead(cueV.artifact)
        val sz0   = af.numFrames.toInt
        val sz    = math.max(1, sz0)
        val buf   = af.buffer(sz)
        try {
          af.read(buf, 0, sz0)
        } finally {
          af.close()
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

        val pd = ProbDist.mk(seed = 0L, tableData = tableData, tableTheta = tableTheta, tablePhi = tablePhi)
        pdRef() = pd
      }
      opt.isDefined
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

      implicit val map: EvtMap = obj.attr

      updateCue()
      // important to make osc-config first!
      mkOscConfig()
      mkGngConfig()
      mkThrottle()
      this
    }

    private def guiInit(): Unit = {
      algorithm.init(createTwo = false)(gngCfg)

      val ggPower = new ToggleButton {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            if (selected) startAlgorithm() else stopAlgorithm()
        }
      }
      val shpPower          = raphael.Shapes.Power _
      ggPower.icon          = GUI.iconNormal  (shpPower)
      ggPower.disabledIcon  = GUI.iconDisabled(shpPower)
      ggPower.tooltip       = "Run/Pause Algorithm"

      val ggOsc = new ToggleButton {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            val r = isAlgorithmRunning
            if (r) stopAlgorithm()
            impl.cursor.step { implicit tx =>
              import TxnLike.{peer => _peer}
              runOsc() = selected
              updateObs()
            }
            if (r) startAlgorithm()
        }
      }
      val shpOsc          = raphael.Shapes.Ethernet _
      ggOsc.icon          = GUI.iconNormal  (shpOsc)
      ggOsc.disabledIcon  = GUI.iconDisabled(shpOsc)
      ggOsc.tooltip       = "Run/Pause OSC Transmission"
      
      val ggChart = new ToggleButton {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            val r = isAlgorithmRunning
            if (r) stopAlgorithm()
            impl.cursor.step { implicit tx =>
              import TxnLike.{peer => _peer}
              runChart() = selected
              updateObs()
            }
            if (r) startAlgorithm()
        }
      }
      val shpChart          = raphael.Shapes.LineChart _
      ggChart.icon          = GUI.iconNormal  (shpChart)
      ggChart.disabledIcon  = GUI.iconDisabled(shpChart)
      ggChart.tooltip       = "Run/Pause Chart Display"
      ggChart.selected      = true

      val ggDumpOsc = new ToggleButton("Dump OSC") {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            impl.cursor.step { implicit tx =>
              import TxnLike.{peer => _peer}
              dumpOsc(selected)
            }
        }
      }

      val ggConsistency = Button("Consistency") {
        val c = algorithm.checkConsistency()
        println(c.pretty)
        val energyInfo = pdRef.single.get match {
          case pd: ProbDist =>
            s"Energy: ${pd.energy}"
          case _ =>
            "(not ProbDist"
        }
        println(energyInfo)
      }

      val pBot = new FlowPanel(ggPower, ggOsc, ggChart, ggConsistency, ggDumpOsc)

      _chart = new AWTChart()
      clearChart()

      /* val mouse = */ ChartLauncher.configureControllers(_chart, "SphereGNG", true, false)
      _chart.render()
//      ChartLauncher.frame(chart, bounds, title)

      val p = new JPanel(new BorderLayout())
      val chartC = _chart.getCanvas.asInstanceOf[java.awt.Component]
      chartC.setPreferredSize(new Dimension(480, 480))
      p.add(BorderLayout.CENTER , chartC)
      p.add(BorderLayout.SOUTH  , pBot.peer)

      component = Component.wrap(p)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      import TxnLike.peer
      observer.dispose()
      stopAlgorithm()
      disposeOscObserver()
      deferTx {
        _chart.dispose()
      }
    }

    // ---- sphere.Observer ----

    private final class OscObserver(val oscT: osc.Transmitter.Undirected.Net, target: InetSocketAddress)
      extends Observer {

      def send(m: osc.Message): Unit = try {
        oscT.send(m, target)
      } catch {
        case NonFatal(ex) =>
          Console.err.println(s"Dropped OSC message:")
          ex.printStackTrace()
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

    private final class ChartObserver extends Observer {
      private final class NodeData(val theta: Double, val phi: Double, val pt: Point)
//      private final case class EdgeData(theta2: Double, phi2: Double, lnStrip: LineStrip)

      private final class LineStripVar(var ln: LineStrip)

      private type EdgeData = Map[Int, LineStripVar]

      private[this] val nodeMap     = mutable.Map.empty[Int, NodeData]
//      private[this] val edgeMap = mutable.Map.empty[Long, EdgeData]
      private[this] val edgeMap     = mutable.Map.empty[Int, EdgeData]

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
        import Math._

        import in._
        val sinTheta  = sin(theta)
        val x         = sinTheta * cos(phi)
        val y         = sinTheta * sin(phi)
        val z         = cos(theta)
        new Coord3d(x, y, z)
      }

      private def centralAngle(theta1: Double, phi1: Double, theta2: Double, phi2: Double): Double = {
        import Math._
        acos(cos(theta1) * cos(theta2) + sin(theta1) * sin(theta2) * cos(phi1 - phi2))
      }

      private def interpolate(theta1: Double, phi1: Double, theta2: Double, phi2: Double, d: Double, f: Double): Coord3d = {
        import Math._
        val PiH     = PI * 0.5
//        val d       = centralAngle(n1, n2)
//        val d = acos(cos(theta1) * cos(theta2) + sin(theta1) * sin(theta2) * cos(phi1 - phi2))

        // http://edwilliams.org/avform.htm
        val lat1    = PiH - theta1
        val lon1    = phi1
        val lat2    = PiH - theta2
        val lon2    = phi2

        val sinD    = sin(d)
        val a       = sin((1 - f) * d) / sinD
        val b       = sin( f      * d) / sinD
        // todo: optimise to use cosTheta, sinTheta
        val cosLat1 = cos(lat1)
        val cosLon1 = cos(lon1)
        val cosLat2 = cos(lat2)
        val cosLon2 = cos(lon2)
        val sinLat1 = sin(lat1)
        val sinLon1 = sin(lon1)
        val sinLat2 = sin(lat2)
        val sinLon2 = sin(lon2)
        val x       = a * cosLat1 * cosLon1 + b * cosLat2 * cosLon2
        val y       = a * cosLat1 * sinLon1 + b * cosLat2 * sinLon2
        val z       = a * sinLat1           + b * sinLat2
//        val lat     = atan2(z, sqrt(x * x + y * y))
//        val lon     = atan2(y, x)
//
//        val theta   = PiH - lat
//        val phi     = lon
//
////        Polar(theta = theta, phi = phi)
//        val sinTheta  = sin(theta)
//        val x         = sinTheta * cos(phi)
//        val y         = sinTheta * sin(phi)
//        val z         = cos(theta)
        new Coord3d(x, y, z)
      }

      private def mkLineStrip(theta1: Double, phi1: Double, theta2: Double, phi2: Double): LineStrip = {
        val d = centralAngle(theta1, phi1, theta2, phi2)
        val numIntp = math.max(2, (d * 18).toInt)
        val c = Vector.tabulate(numIntp) { i =>
          val f = i.toDouble / (numIntp - 1)
          val p = interpolate(theta1, phi1, theta2, phi2, d, f)
          p // mkCoord(p)
        }

        val ln = new LineStrip(c: _*)
        ln.setWireframeColor(Color.BLACK)
        ln
      }

      private abstract class Cmd {
        def execute(redraw: Boolean): Unit
      }

      private[this] val commands = new ConcurrentLinkedDeque[Cmd]()

      private[this] object flush extends Runnable {
        def run(): Unit = {
          while ({
            val cmd = commands.pollLast()
            val ok  = cmd != null
            if (ok) cmd.execute(redraw = commands.isEmpty)
            ok
          }) ()
        }
      }

      private def push(cmd: Cmd): Unit = {
        val refresh = commands.isEmpty
        commands.push(cmd)
        if (refresh) EventQueue.invokeLater(flush)
      }

      def gngNodeUpdated(n: Node): Unit = {
        val nid     = n.id
        val theta1  = n.theta
        val phi1    = n.phi
        val ptNew   = new Point(mkCoord(n), Color.RED, 3f)
        push(new CmdNodeUpdate(nid, theta1, phi1, ptNew))
      }

      private final class CmdNodeUpdate(nid: Int, theta1: Double, phi1: Double, ptNew: Point) extends Cmd {
        def execute(redraw: Boolean): Unit = {
          nodeMap.get(nid).foreach { data =>
            _chart.removeDrawable(data.pt, false)
          }
          nodeMap(nid) = new NodeData(theta1, phi1, ptNew)
          val edges = edgeMap.getOrElse(nid, Map.empty)
          edges.iterator.foreach { case (thatId, lineVar) =>
            nodeMap.get(thatId).foreach { nodeData =>
              val theta2  = nodeData.theta
              val phi2    = nodeData.phi
              val lnNew   = mkLineStrip(theta1, phi1, theta2, phi2)
              val lnOld   = lineVar.ln
              lineVar.ln  = lnNew
              if (lnOld != null) _chart.removeDrawable(lnOld, false)
              _chart.add(lnNew, false)
            }
          }
          _chart.add(ptNew, redraw)
          //        setScale()
          // log(s"gngNodeUpdated $ptOld -> $ptNew")
        }
      }

      def gngNodeInserted(n: Node): Unit = {
        val nid   = n.id
        val theta = n.theta
        val phi   = n.phi
        val ptNew = new Point(mkCoord(n), Color.RED, 3f)
        push(new CmdNodeInsert(nid, theta, phi, ptNew))
      }

      private final class CmdNodeInsert(nid: Int, theta: Double, phi: Double, ptNew: Point) extends Cmd {
        def execute(redraw: Boolean): Unit = {
          nodeMap(nid) = new NodeData(theta, phi, ptNew)
          _chart.add(ptNew, redraw)
        }
      }

      def gngNodeRemoved(n: Node): Unit = {
        val nid = n.id
        push(new CmdNodeRemove(nid))
      }

      private final class CmdNodeRemove(nid: Int) extends Cmd {
        def execute(redraw: Boolean): Unit = {
          edgeMap.get(nid).foreach { m =>
            m.keysIterator.foreach { toId =>
              removeEdge(nid, toId, redraw = false)
            }
          }
          nodeMap.remove(nid).foreach { data =>
            _chart.removeDrawable(data.pt, redraw)
          }
        }
      }

      // we do not visualise 'age'
      def gngEdgeUpdated(e: Edge): Unit = ()

      def gngEdgeInserted(e: Edge): Unit = {
        val from    = e.from
        val fromId  = from.id
        val to      = e.to
        val toId    = to  .id
        val theta1  = from.theta
        val phi1    = from.phi
        val theta2  = to  .theta
        val phi2    = to  .phi
        push(new CmdEdgeInsert(fromId, toId, theta1, phi1, theta2, phi2))
      }

      private final class CmdEdgeInsert(fromId: Int, toId: Int, theta1: Double, phi1: Double, theta2: Double, phi2: Double)
        extends Cmd {

        def execute(redraw: Boolean): Unit = {
          val lnNew = mkLineStrip(theta1, phi1, theta2, phi2)
          val vr    = new LineStripVar(lnNew)
          val m00   = edgeMap.getOrElse(fromId, Map.empty)
          val m01   = m00 + (toId -> vr)
          edgeMap(fromId) = m01
          val m10   = edgeMap.getOrElse(toId  , Map.empty)
          val m11   = m10 + (fromId -> vr)
          edgeMap(toId  ) = m11
          _chart.add(lnNew, redraw)
        }
      }

      def gngEdgeRemoved(e: Edge): Unit = {
        val from    = e.from
        val fromId  = from.id
        val to      = e.to
        val toId    = to  .id
        push(new CmdEdgeRemove(fromId, toId))
      }

      private final class CmdEdgeRemove(fromId: Int, toId: Int) extends Cmd {
        def execute(redraw: Boolean): Unit =
          removeEdge(fromId, toId, redraw = redraw)
      }

      private def removeEdge(fromId: Int, toId: Int, redraw: Boolean): Unit = {
        val m00 = edgeMap.getOrElse(fromId, Map.empty)
        m00.get(toId).foreach { lnVar =>
          if (lnVar.ln != null) _chart.removeDrawable(lnVar.ln, redraw)
          lnVar.ln == null
        }
        val m01 = m00 - toId
        edgeMap(fromId) = m01

        val m10 = edgeMap.getOrElse(toId  , Map.empty)
        m10.get(toId).foreach { lnVar =>
          if (lnVar.ln != null) _chart.removeDrawable(lnVar.ln, redraw)
          lnVar.ln == null
        }
        val m11 = m10 - fromId
        edgeMap(toId  ) = m11
      }
    }

    private final class MultiObserver(a: Observer, b: Observer) extends Observer {
      def gngNodeUpdated  (n: Node): Unit = { a.gngNodeUpdated  (n);  b.gngNodeUpdated  (n) }
      def gngEdgeUpdated  (e: Edge): Unit = { a.gngEdgeUpdated  (e);  b.gngEdgeUpdated  (e) }
      def gngNodeInserted (n: Node): Unit = { a.gngNodeInserted (n);  b.gngNodeInserted (n) }
      def gngEdgeInserted (e: Edge): Unit = { a.gngEdgeInserted (e);  b.gngEdgeInserted (e) }
      def gngNodeRemoved  (n: Node): Unit = { a.gngNodeRemoved  (n);  b.gngNodeRemoved  (n) }
      def gngEdgeRemoved  (e: Edge): Unit = { a.gngEdgeRemoved  (e);  b.gngEdgeRemoved  (e) }
    }
  }
}
