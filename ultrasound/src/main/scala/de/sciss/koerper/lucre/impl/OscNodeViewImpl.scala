/*
 *  OscNodeViewImpl.scala
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

import java.util.regex.Pattern

import de.sciss.icons.raphael
import de.sciss.koerper.Koerper
import de.sciss.koerper.lucre.impl.OscNodeImpl.Stateful
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.{stm, event => evt}
import de.sciss.mellite.gui.GUI
import de.sciss.osc
import de.sciss.synth.proc.Workspace

import scala.swing.event.ButtonClicked
import scala.swing.{Action, BorderPanel, Component, FlowPanel, Label, TextField, ToggleButton}

object OscNodeViewImpl {
  def apply[S <: Sys[S]](n: OscNode[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                        workspace: Workspace[S]): OscNodeView[S] = {
    val state = OscNodeImpl.stateful(n)
    new Impl[S](state).init(n)
  }

  private final case class OscConfig(targetHost: String = Koerper.IpDavid, targetPort: Int = Koerper.OscPortDavid,
                                     localHost: String = "", localPort: Int = 0,
                                     transport: osc.Transport = osc.UDP)

  private final class Impl[S <: Sys[S]](state: Stateful[S])
                                       (implicit val cursor: stm.Cursor[S], val workspace: Workspace[S])
    extends OscNodeView[S] with ComponentHolder[Component] { impl =>

    private[this] var observer: Disposable[S#Tx] = _

    private type EvtMap = evt.Map[S, String, Obj]

    def init(obj: OscNode[S])(implicit tx: S#Tx): this.type = {
      deferTx(guiInit())
      observer = obj.attr.changed.react { implicit tx => upd =>
        upd.changes.foreach {
//          case Obj.AttrAdded    (key, _)    => checkUpdate(upd.map, key)
//          case Obj.AttrRemoved  (key, _)    => checkUpdate(upd.map, key)
//          case Obj.AttrReplaced (key, _, _) => checkUpdate(upd.map, key)
          case _ =>
        }
      }

//      implicit val map: EvtMap = obj.attr
//
//      updateCue()
      this
    }

    private[this] val pat = Pattern.compile("([^\"]\\S*|\".+?\")\\s*")

    private def mkMessage(s: String): Option[osc.Message] = {
      val m = pat.matcher(s)
      val res = Seq.newBuilder[Any]
      if (!m.find()) return None
      val cmd = m.group(1)
      while (m.find()) {
        val sub = m.group(1)
        // XXX TODO --- cheesy way of parsing
        val atom: Any = try {
          sub.toInt
        } catch {
          case _: NumberFormatException =>
            try {
              sub.toFloat
            } catch {
              case _: NumberFormatException =>
                try {
                  sub.toBoolean
                } catch {
                  case _: IllegalArgumentException =>
                    if (sub.length >= 2 && sub.charAt(0) == '"' && sub.charAt(sub.length - 1) == '"') {
                      sub.substring(1, sub.length - 1)
                    } else {
                      sub
                    }
                }
            }
        }
        res += atom
      }
      val msg = osc.Message(cmd, res.result(): _*)
      Some(msg)
    }

    private def guiInit(): Unit = {
      val lbMessage = new Label("Message:")
      val ggMessage = new TextField(20)

      val actionSend = Action("Send") {
        mkMessage(ggMessage.text).foreach { m =>
          state ! m
        }
      }
      val ggSend = GUI.toolButton(actionSend, raphael.Shapes.Mail, "Send")

      val ggDump = new ToggleButton("Dump") {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) =>
            val mode = if (selected) osc.Dump.Text else osc.Dump.Off
            state.dump(mode)
        }
      }
      val shpDump          = raphael.Shapes.Printer _ // Terminal _
      ggDump.icon          = GUI.iconNormal  (shpDump)
      ggDump.disabledIcon  = GUI.iconDisabled(shpDump)
      ggDump.tooltip       = "Dump OSC Packets"

      val pCenter = new FlowPanel(lbMessage, ggMessage, ggSend)
      val pBot    = new FlowPanel(ggDump)

      val p = new BorderPanel {
        add(pCenter, BorderPanel.Position.Center)
        add(pBot   , BorderPanel.Position.South )
      }

      component = p
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
    }
  }
}
