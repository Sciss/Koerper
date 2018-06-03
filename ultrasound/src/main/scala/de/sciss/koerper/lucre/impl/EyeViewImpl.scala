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

import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Color}

import de.sciss.icons.raphael
import de.sciss.koerper.Koerper
import de.sciss.lucre.stm.{Disposable, Obj, Sys, TxnLike}
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.{stm, event => evt}
import de.sciss.mellite.gui.GUI
import de.sciss.neuralgas.sphere.PD
import de.sciss.neuralgas.sphere.impl.LocVarImpl
import de.sciss.numbers.DoubleFunctions
import de.sciss.osc
import de.sciss.processor.Processor
import de.sciss.synth.proc.{AudioCue, SoundProcesses, Workspace}
import javax.swing.JPanel

import scala.concurrent.stm.{Ref, atomic}
import scala.swing.event.ButtonClicked
import scala.swing.{Component, Dimension, FlowPanel, Graphics2D, ToggleButton}

object EyeViewImpl {
  def apply[S <: Sys[S]](obj: Eye[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                            workspace: Workspace[S]): EyeView[S] = {
    new Impl[S].init(obj)
  }

  private final case class OscConfig(targetHost: String = Koerper.IpDavid, targetPort: Int = Koerper.OscPortDavid,
                                     localHost: String = "", localPort: Int = 0,
                                     transport: osc.Transport = osc.UDP)

  private final class Impl[S <: Sys[S]](implicit val cursor: stm.Cursor[S], val workspace: Workspace[S])
    extends EyeView[S] with ComponentHolder[Component] { impl =>

    private[this] var observer: Disposable[S#Tx] = _

    @volatile
    private[this] var pdRef: PD = PD.Uniform

    private[this] var img: BufferedImage = _

    private type EvtMap = evt.Map[S, String, Obj]

    private def checkUpdate(map: EvtMap, key: String)(implicit tx: S#Tx): Unit = {
      implicit val _map: EvtMap = map
      if (key == Eye.attrTable) {
        updateCue()
      }
    }

    private[this] val timerRef = Ref(Option.empty[javax.swing.Timer])

//    private def isAlgorithmRunning: Boolean = timerRef.single.get.isDefined

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

    private[this] val loadCueRef = Ref(Option.empty[Processor[PD]])

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
          pdRef = pd
        }
        p.onComplete { _ =>
          atomic { implicit tx =>
            if (loadCueRef().contains(p)) loadCueRef() = None
          }
        }
      }
    }

    def init(obj: Eye[S])(implicit tx: S#Tx): this.type = {
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
      this
    }

    private[this] var rotX  = 0.0
    private[this] var rotY  = 0.0

    private[this] final val Pi2 = math.Pi * 2
    private[this] final val rotXD = 0.023.toRadians
    private[this] final val rotYD = 0.011.toRadians

    private[this] val locVar = new LocVarImpl

//    private[this] val oval = new Ellipse2D.Double

    private[this] final val cosTable = Array.tabulate(32768)(i => Math.cos(i * Math.PI / 65536))

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

    private[this] final val PiH = Math.PI / 2

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
      val N = 192000  // XXX TODO --- scale according to ProbDist.energy
      var i = 0
      val _rotX = rotX
      val _rotY = rotY
      val _loc = locVar
//      val _oval = oval
      val pd = pdRef
      while (i < N) {
        pd.poll(_loc)
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

      override protected def paintComponent(g: Graphics2D): Unit = {
//        super.paintComponent(g)
        g.setColor(Color.black)
        val p = peer
        val w = p.getWidth
        val h = p.getHeight
        if (w < h) {
          val m = (h - w)/2
          g.drawImage(img, 0, m, w, w, p)
        } else {
          val m = (w - h)/2
          g.drawImage(img, m, 0, h, h, p)
        }
      }
    }

    private[this] final val EXT   = 1080
    private[this] final val EXTM  = EXT - 1

    private def guiInit(): Unit = {
//      algorithm.init(createTwo = false)(gngCfg)

      img = new BufferedImage(EXT, EXT, BufferedImage.TYPE_BYTE_BINARY) // TYPE_INT_ARGB)
      val g = img.createGraphics()
      g.setColor(Color.black)
      g.fillRect(0, 0, EXT, EXT)
      g.dispose()

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

      val pBot = new FlowPanel(ggPower)

      val p = new JPanel(new BorderLayout())
      p.add(BorderLayout.CENTER , (new C).peer)
      p.add(BorderLayout.SOUTH  , pBot.peer)

      component = Component.wrap(p)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
      stopAlgorithm()
      cancelLoad()
    }
  }
}
