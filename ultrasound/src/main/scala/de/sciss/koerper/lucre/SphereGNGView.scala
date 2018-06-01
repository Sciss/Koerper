package de.sciss.koerper.lucre

import de.sciss.lucre.stm.Sys
import de.sciss.mellite.gui.ViewHasWorkspace

object SphereGNGView {
//  def apply(config: Config): SphereGNGView = impl.SphereGNGImpl(config)

  def apply[S <: Sys[S]](obj: SphereGNG[S])(implicit tx: S#Tx): SphereGNGView[S] = impl.SphereGNGViewImpl(obj)
}
trait SphereGNGView[S <: Sys[S]] extends ViewHasWorkspace[S] {
  def step(): Unit
}
