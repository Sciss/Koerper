package de.sciss.koerper.lucre.impl

import de.sciss.koerper.lucre.{OscNode, OscNodeFrame, OscNodeView}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.{CellView, View}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.AttrCellView
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.synth.proc.Workspace

object OscNodeFrameImpl {
  def apply[S <: Sys[S]](obj: OscNode[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): OscNodeFrame[S] = {
//    implicit val undoMgr: UndoManager = UndoManager()
    val oscNodeView = OscNodeViewImpl(obj)
    val name        = AttrCellView.name(obj)
    val res         = new FrameImpl[S](oscNodeView, name)
    res.init()
    res
  }

  private final class FrameImpl[S <: Sys[S]](val oscNodeView: OscNodeView[S], name: CellView[S#Tx, String])
    extends WindowImpl[S](name) with OscNodeFrame[S] {

    def view: View[S] = oscNodeView
  }
}