/*
 *  EyeViewImpl.scala
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

import java.awt.Color
import java.awt.event.{ActionEvent, KeyEvent}
import java.awt.image.BufferedImage

import de.sciss.koerper.Koerper
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys, TxnLike}
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.neuralgas.sphere.PD
import de.sciss.neuralgas.sphere.impl.LocVarImpl
import de.sciss.numbers.DoubleFunctions
import de.sciss.osc
import de.sciss.processor.Processor
import de.sciss.synth.proc.{AudioCue, SoundProcesses, Workspace}
import javax.swing.{AbstractAction, JComponent, KeyStroke}

import scala.concurrent.stm.{Ref, atomic}
import scala.swing.{Component, Dimension, Graphics2D}

object EyeViewImpl {
  def apply[S <: Sys[S]](obj: Eye[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      workspace: Workspace[S]): EyeView[S] = {
    new Impl[S].init(obj)
  }

  private final class PdFader(val a: PD, val b: PD, val startTime: Long, val phase: Boolean,
                              var nA: Int, var nB: Int)

  private final case class OscConfig(targetHost: String = Koerper.IpDavid, targetPort: Int = Koerper.OscPortDavid,
                                     localHost: String = "", localPort: Int = 0,
                                     transport: osc.Transport = osc.UDP)

  private final class Impl[S <: Sys[S]](implicit val cursor: stm.Cursor[S], val workspace: Workspace[S])
    extends EyeView[S] with MultiAttrObserver[S] with ComponentHolder[Component] { impl =>

    @volatile
    private[this] var pdRef: PdFader =
      new PdFader(PD.Uniform, PD.Uniform, startTime = System.currentTimeMillis(), phase = false,
        nA = 1000, nB = 1000)

    private[this] var img: BufferedImage = _

    private[this] val timerRef    = Ref(Option.empty[javax.swing.Timer])
    private[this] val runState    = Ref(false)
    private[this] val loadCueRef  = Ref(Option.empty[Processor[PD]])

    private[this] var objH: stm.Source[S#Tx, Eye[S]] = _

    private[this] var rotX  = 0.0
    private[this] var rotY  = 0.0

    private[this] final val Pi2   = math.Pi * 2
    private[this] final val rotXD = 0.023.toRadians
    private[this] final val rotYD = 0.011.toRadians

    private[this] val locVar = new LocVarImpl

    private[this] final val cosTable = Array.tabulate(32768)(i => Math.cos(i * Math.PI / 65536))

    private[this] final val PiH   = Math.PI / 2

    private[this] var EXT   = 1080/2 // 480 // 1080
    private[this] var EXTM  = EXT - 1

    @volatile
    private[this] var fadeTime = 1.0f

    @volatile
    private[this] var maxPoints = 1

    @volatile
    private[this] var pointFraction = 1.0

    private[this] val rnd = new java.util.Random(pdRef.startTime)

    protected def multiAttrKeys: Set[String] = Eye.attrAll

    protected def multiAttrMap(implicit tx: S#Tx): EvtMap = objH().attr

    private def mkN()(implicit tx: S#Tx, map: EvtMap): Unit = {
      maxPoints       = math.max(0, getInt(Eye.attrMaxPoints, 192000))
      pointFraction   = math.max(0.0, math.min(1.0, getDouble(Eye.attrPointFraction, 0.5)))
      val pd0 = pdRef
      pd0.nA                = pd0.a match {
        case _pd: ProbDist  => math.min(maxPoints, (_pd.energy * pointFraction).toInt)
        case _              => maxPoints
      }
      pd0.nB                = pd0.b match {
        case _pd: ProbDist  => math.min(maxPoints, (_pd.energy * pointFraction).toInt)
        case _              => maxPoints
      }
    }

    @inline
    private def mkFadeTime()(implicit tx: S#Tx, map: EvtMap): Unit = {
      fadeTime = math.max(0.0f, math.min(480.0f, getDouble(Eye.attrFadeTime, 60.0).toFloat))
    }

    /** @return `true` if the value should be observed. */
    protected def checkMultiAttrUpdate(map: EvtMap, key: String, value: Obj[S])(implicit tx: S#Tx): Boolean = {
      implicit val _map: EvtMap = map
      if (key == Eye.attrTable) {
        updateCue()
        false
      } else if (key == Eye.attrMaxPoints || key == Eye.attrPointFraction) {
        mkN()
        true
      } else if (key == Eye.attrFadeTime) {
        mkFadeTime()
        true
      } else {
        false
      }
    }

    def run(implicit tx: S#Tx): Boolean = {
      import TxnLike.peer
      runState()
    }

    def run_=(value: Boolean)(implicit tx: S#Tx): Unit = {
      import TxnLike.peer
      val oldState = runState.swap(value)
      if (oldState != value) deferTx {
        if (value) startAlgorithm() else stopAlgorithm()
      }
    }

    private def startAlgorithm(): Unit = {
      val fps = 25.0
      val dly = math.max(1, (1000 / math.max(1, fps)).toInt)
      val t   = new javax.swing.Timer(dly, { _ =>
        updateImage()
        component.repaint()
      })
      t.setRepeats(true)
//      t.setCoalesce(true)
      timerRef.single.swap(Some(t)).foreach(_.stop())
      t.start()
    }

    private def stopAlgorithm(): Unit = {
      timerRef.single.swap(None).foreach(_.stop())
    }

    private def cancelLoad()(implicit tx: TxnLike): Unit =
      loadCueRef.swap(None)(tx.peer).foreach { pOld => tx.afterCommit(pOld.abort()) }

    private def updateCue()(implicit tx: S#Tx, map: EvtMap): Unit = {
//      import TxnLike.peer
      val opt = map.$[AudioCue.Obj](Eye.attrTable)
      opt.foreach { cue =>
        cancelLoad()
        val f = cue.value.artifact
        val p = ProbDist.read(f)
        loadCueRef.set(Some(p))(tx.peer)
        import SoundProcesses.executionContext
        p.foreach { pd =>
          val pd0 = pdRef
          val t   = System.currentTimeMillis()
          val n   = math.min(maxPoints, (pd.energy * pointFraction).toInt)
          val pd1 = if (pd0.phase) {
            new PdFader(pd0.a, pd, t, phase = false, nA = pd0.nA, nB = n)
          } else {
            new PdFader(pd, pd0.b, t, phase = true , nA = n, nB = pd0.nB)
          }
          pdRef   = pd1
        }
        p.onComplete { _ =>
          atomic { implicit tx =>
            if (loadCueRef().contains(p)) loadCueRef() = None
          }
        }
      }
    }

    def init(obj: Eye[S])(implicit tx: S#Tx): this.type = {
      objH = tx.newHandle(obj)
      implicit val map: EvtMap = obj.attr

      map.$[IntObj](Eye.attrExtent).foreach { extObj =>
        EXT   = extObj.value
        EXTM  = EXT - 1
      }

      deferTx(guiInit())
      initMultiAttr(obj)

      updateCue()
      mkN()
      mkFadeTime()
      this
    }

    private def cosF(in: Double): Double = {
      val w = DoubleFunctions.wrap(in, 0.0, Pi2)
      val j = (w * 65536 / Math.PI).toInt
      if (j < 32768) {
        cosTable(j)

      } else if (j < 65536) {
        val k = 65535 - j
        -cosTable(k)

      } else if (j < 98304) {
        val k = j - 65536
        -cosTable(k)

      } else {
//        if (j > 131072) {
//          println(s"WTF, in = $in, w = $w, j = $j")
//          return 0.0
//        }

        val k = 131071 - j
        cosTable(k)
      }
    }

    private def sinF(in: Double): Double = {
      cosF(in - PiH)
    }

    private def updateImage(): Unit = {
      val _img = img
      val g = _img.createGraphics()
//      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
//      g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )
      g.setColor(Color.black)
      g.fillRect(0, 0, EXT, EXT)
      g.setColor(Color.white)
      val pd  = pdRef
      var i = 0
      val _rotX = rotX
      val _rotY = rotY
      val _loc = locVar
//      val _oval = oval
      val pdA = if (pd.phase) pd.a  else pd.b
      val pdB = if (pd.phase) pd.b  else pd.a
      val nA  = if (pd.phase) pd.nA else pd.nB
      val nB  = if (pd.phase) pd.nB else pd.nA
      val t1  = System.currentTimeMillis()
      val pw  = math.max(0.0f, math.min(1.0f, (t1 - pd.startTime) * 0.001f / fadeTime))
      val _N  = (nA * pw + nB * (1 - pw)).toInt

      while (i < _N) {
        val pd1 = if (rnd.nextFloat() < pw) pdA else pdB
        pd1.poll(_loc)
        val sinTheta = sinF(_loc.theta)
        val x0 = sinTheta * cosF(_loc.phi)
        val y0 = sinTheta * sinF(_loc.phi)
        val z0 = cosF(_loc.theta)

        val cosRX = cosF(_rotX)
        val sinRX = sinF(_rotX)
        val x1    = x0
        val y1    = y0*cosRX - z0*sinRX
        val z1    = y0*sinRX + z0*cosRX

        val cosRY = cosF(_rotY)
        val sinRY = sinF(_rotY)
        val x2    = x1*cosRY - z1*sinRY
        val y2    = y1
        val z2    = x1*sinRY + z1*cosRY

        val x =  x2 // x0
        val y =  y2 // y0
        val z =  z2 // z0

        if (z < 0) {
          val xi = (x * 0.5 + 0.5) * EXTM
          val yi = (y * 0.5 + 0.5) * EXTM
//          _oval.setFrame(xi - 1.0, yi - 1.0, 2.0, 2.0)
//          g.fill(_oval)

          _img.setRGB(xi.toInt, yi.toInt, 0xFFFFFFFF)

          //        count += 1
        }

        i += 1
      }
      g.dispose()

      rotX = (rotX + rotXD) % Pi2
      rotY = (rotY + rotYD) % Pi2
    }

    private final class C extends Component {
      preferredSize = new Dimension(EXT/2, EXT/2)
//      background    = Color.black
      opaque        = true

      override protected def paintComponent(g: Graphics2D): Unit = {
//        super.paintComponent(g)
        g.setColor(Color.black)
        val p = peer
        val w = p.getWidth
        val h = p.getHeight
        g.fillRect(0, 0, w, h)
        if (w < h) {
          val m = (h - w)/2
          g.drawImage(img, 0, m, w, w, p)
        } else {
          val m = (w - h)/2
          g.drawImage(img, m, 0, h, h, p)
        }
      }
    }

    private def guiInit(): Unit = {
//      algorithm.init(createTwo = false)(gngCfg)

      img = new BufferedImage(EXT, EXT, BufferedImage.TYPE_BYTE_BINARY) // TYPE_INT_ARGB)
      val g = img.createGraphics()
      g.setColor(Color.black)
      g.fillRect(0, 0, EXT, EXT)
      g.dispose()

      val c       = new C
      val display = c.peer

      val iMap    = display.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
      val aMap    = display.getActionMap
      val runName = "run"
      iMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, 0), runName)
      aMap.put(runName, new AbstractAction(runName) {
        def actionPerformed(e: ActionEvent): Unit = {
//          val _run = selected
          impl.cursor.step { implicit tx =>
            run = !run
          }
        }
      })

      component = c
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      disposeMultiAttrObserver()
      stopAlgorithm()
      cancelLoad()
    }
  }
}
