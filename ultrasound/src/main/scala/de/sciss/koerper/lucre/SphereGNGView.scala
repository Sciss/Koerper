/*
 *  SphereGNGView.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.mellite.gui.ViewHasWorkspace
import de.sciss.synth.proc.Workspace

object SphereGNGView {
  def apply[S <: Sys[S]](obj: SphereGNG[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                            workspace: Workspace[S]): SphereGNGView[S] = impl.SphereGNGViewImpl(obj)
}
trait SphereGNGView[S <: Sys[S]] extends ViewHasWorkspace[S] {
  def run(implicit tx: S#Tx): Boolean

  def run_=(value: Boolean)(implicit tx: S#Tx): Unit
}
