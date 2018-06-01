package de.sciss.koerper.lucre
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.synth.proc.Workspace

import scala.swing.Component

object SphereGNGViewImpl {
  def apply[S <: Sys[S]](obj: SphereGNG[S])(implicit tx: S#Tx): SphereGNGView[S] = {
    ???
  }

  private final class Impl[S <: Sys[S]](implicit val cursor: stm.Cursor[S], val workspace: Workspace[S])
    extends SphereGNGView[S] with ComponentHolder[Component] {
    def step(): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???
  }
}
