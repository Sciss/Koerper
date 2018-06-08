package de.sciss.koerper.lucre

import de.sciss.koerper.lucre.impl.{SphereGNGFrameImpl => Impl}
import de.sciss.lucre
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Workspace

object SphereGNGFrame {
  def apply[S <: Sys[S]](n: SphereGNG[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): SphereGNGFrame[S] =
    Impl(n)
}

trait SphereGNGFrame[S <: Sys[S]] extends lucre.swing.Window[S] {
  def sphereView: SphereGNGView[S]

  def run(implicit tx: S#Tx): Boolean

  def run_=(value: Boolean)(implicit tx: S#Tx): Unit
}