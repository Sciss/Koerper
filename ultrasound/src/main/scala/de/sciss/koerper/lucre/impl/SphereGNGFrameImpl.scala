package de.sciss.koerper.lucre.impl

import de.sciss.koerper.lucre.{SphereGNG, SphereGNGFrame, SphereGNGView}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.{CellView, View}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.AttrCellView
import de.sciss.mellite.gui.impl.WindowImpl
import de.sciss.synth.proc.Workspace

object SphereGNGFrameImpl {
  def apply[S <: Sys[S]](obj: SphereGNG[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): SphereGNGFrame[S] = {
    //    implicit val undoMgr: UndoManager = UndoManager()
    val sphereView  = SphereGNGView(obj)
    val name        = AttrCellView.name(obj)
    val res         = new FrameImpl[S](sphereView, name)
    res.init()
    res
  }

  private final class FrameImpl[S <: Sys[S]](val sphereView: SphereGNGView[S], name: CellView[S#Tx, String])
    extends WindowImpl[S](name) with SphereGNGFrame[S] {

    def view: View[S] = sphereView

    def run(implicit tx: S#Tx): Boolean =
      sphereView.run

    def run_=(value: Boolean)(implicit tx: S#Tx): Unit =
      sphereView.run = value
  }
}