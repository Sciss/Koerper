/*
 *  OscNodeView.scala
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

object OscNodeView {
  def apply[S <: Sys[S]](obj: OscNode[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                          workspace: Workspace[S]): OscNodeView[S] = impl.OscNodeViewImpl(obj)
}
trait OscNodeView[S <: Sys[S]] extends ViewHasWorkspace[S]