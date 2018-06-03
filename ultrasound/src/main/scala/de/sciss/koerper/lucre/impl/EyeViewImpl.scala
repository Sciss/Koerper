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

import java.awt.BorderLayout
import java.util

import de.sciss.icons.raphael
import de.sciss.koerper.Koerper
import de.sciss.lucre.stm.{Disposable, Obj, Sys, TxnLike}
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.{stm, event => evt}
import de.sciss.mellite.gui.GUI
import de.sciss.neuralgas.sphere.{LocVar, PD}
import de.sciss.osc
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{AudioCue, Workspace}
import javax.swing.JPanel

import scala.concurrent.stm.Ref
import scala.swing.event.ButtonClicked
import scala.swing.{Component, FlowPanel, ToggleButton}

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

    private[this] val pdRef       = Ref[PD](PD.Uniform)

    private type EvtMap = evt.Map[S, String, Obj]

    private def checkUpdate(map: EvtMap, key: String)(implicit tx: S#Tx): Unit = {
      implicit val _map: EvtMap = map
      if (key == Eye.attrTable) {
        if (updateCue()) {
          // mkGngConfig()
        }
      }
    }

    private[this] val timerRef = Ref(Option.empty[java.util.Timer])

    private def isAlgorithmRunning: Boolean = timerRef.single.get.isDefined

    private def startAlgorithm(): Unit = {
    }

    private def stopAlgorithm(): Unit = {
    }

    private def updateCue()(implicit tx: S#Tx, map: EvtMap): Boolean = {
      import TxnLike.peer
      val opt = map.$[AudioCue.Obj](Eye.attrTable)
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

        val pd = new ProbDist(seed = 0L, tableData = tableData, tableTheta = tableTheta, tablePhi = tablePhi)
        pdRef() = pd
      }
      opt.isDefined
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

    private def guiInit(): Unit = {
//      algorithm.init(createTwo = false)(gngCfg)

      val ggPower = new ToggleButton {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            // if (selected) startAlgorithm() else stopAlgorithm()
        }
      }
      val shpPower          = raphael.Shapes.Power _
      ggPower.icon          = GUI.iconNormal  (shpPower)
      ggPower.disabledIcon  = GUI.iconDisabled(shpPower)
      ggPower.tooltip       = "Run/Pause Algorithm"

      val pBot = new FlowPanel(ggPower)

      val p = new JPanel(new BorderLayout())
//      p.add(BorderLayout.CENTER , chartC)
      p.add(BorderLayout.SOUTH  , pBot.peer)

      component = Component.wrap(p)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
//      stopAlgorithm()
    }

    // ---- PD ----

    private final class ProbDist(seed: Long, tableData: Array[Float],
                                 tableTheta: Array[Float], tablePhi: Array[Float]) extends PD {
      private[this] val rnd = new util.Random(seed)
      private[this] val sz  = tableData.length
      private[this] val max = {
        val x = tableData(sz - 1)
        if (x > 0f) x else 0.1f
      }

      def poll(loc: LocVar): Unit = {
        val i0    = util.Arrays.binarySearch(tableData, 0, sz, rnd.nextFloat() * max)
        val i1    = if (i0 >= 0) i0 else -(i0 - 1)
        val dot   = if (i1 < tableData.length) i1 else sz - 1
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
  }
}
