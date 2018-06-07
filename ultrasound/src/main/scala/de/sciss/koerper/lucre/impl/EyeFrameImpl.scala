package de.sciss.koerper.lucre.impl

import java.awt.Toolkit
import java.awt.event.{ActionEvent, InputEvent, KeyEvent}

import de.sciss.desktop
import de.sciss.desktop.Window
import de.sciss.koerper.lucre.{Eye, EyeFrame, EyeView}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.swing.{CellView, View, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.AttrCellView
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.synth.proc.Workspace
import javax.swing.{AbstractAction, JComponent, KeyStroke}

import scala.concurrent.stm.Ref

object EyeFrameImpl {
  def apply[S <: Sys[S]](obj: Eye[S], undecorated: Boolean)
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): EyeFrame[S] = {
//    implicit val undoMgr: UndoManager = UndoManager()
    val eyeView = EyeView(obj)
    val name    = AttrCellView.name(obj)
    val res     = new FrameImpl[S](eyeView, name, undecorated = undecorated)
    res.init()
    res
  }

  private final class FrameImpl[S <: Sys[S]](val eyeView: EyeView[S], name: CellView[S#Tx, String],
                                             override val undecorated: Boolean)
    extends WindowImpl[S](name) with EyeFrame[S] { impl =>

    def view: View[S] = eyeView

    private[this] val fsState = Ref(false)

    override protected def initGUI(): Unit = {
      super.initGUI()
      installFullscreenKey()
    }

    private def findAWT(): Option[java.awt.Window] =
      window.component.peer match {
        case frame: java.awt.Window => Some(frame)
        case _ => None
      }

    private def installFullscreenKey(): Unit = {
      val display = view.component.peer
      val iMap    = display.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW)
      val aMap    = display.getActionMap

      val fsName  = "fullscreen"
      iMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_F, Toolkit.getDefaultToolkit.getMenuShortcutKeyMask |
        InputEvent.SHIFT_MASK), fsName)
      aMap.put(fsName, new AbstractAction(fsName) {
        def actionPerformed(e: ActionEvent): Unit = {
          eyeView.cursor.step { implicit tx =>
            fullscreen = !fullscreen
          }
        }
      })

      val closeName = "close"
      iMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_W, Toolkit.getDefaultToolkit.getMenuShortcutKeyMask), closeName)
      aMap.put(closeName, new AbstractAction(fsName) {
        def actionPerformed(e: ActionEvent): Unit = {
          eyeView.cursor.step { implicit tx =>
            impl.dispose()
          }
        }
      })
    }

    override protected def style: Window.Style = desktop.Window.Auxiliary // i.e. no menu bar

    def fullscreen(implicit tx: S#Tx): Boolean = {
      fsState.get(tx.peer)
    }

    def fullscreen_=(value: Boolean)(implicit tx: S#Tx): Unit = {
      import TxnLike.peer
      val oldValue = fsState.swap(value)
      if (oldValue != value) deferTx {
        findAWT().fold[Unit](Console.err.println("Warning: fullscreen - cannot find AWT peer")) { frame =>
          val gc = frame.getGraphicsConfiguration
          val sd = gc.getDevice
          sd.setFullScreenWindow(if (value) frame else null)
        }
      }
    }

    def run(implicit tx: S#Tx): Boolean =
      eyeView.run

    def run_=(value: Boolean)(implicit tx: S#Tx): Unit =
      eyeView.run = value
  }
}