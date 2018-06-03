/*
 *  EyeView.scala
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

object EyeView {
  def apply[S <: Sys[S]](obj: Eye[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      workspace: Workspace[S]): EyeView[S] = impl.EyeViewImpl(obj)
}
trait EyeView[S <: Sys[S]] extends ViewHasWorkspace[S] {
}
