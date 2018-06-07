package de.sciss.koerper.lucre

import de.sciss.koerper.lucre.impl.{OscNodeFrameImpl => Impl}
import de.sciss.lucre
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Workspace

object OscNodeFrame {
  def apply[S <: Sys[S]](n: OscNode[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): OscNodeFrame[S] =
    Impl(n)
}

trait OscNodeFrame[S <: Sys[S]] extends lucre.swing.Window[S] {
  def oscNodeView: OscNodeView[S]
}