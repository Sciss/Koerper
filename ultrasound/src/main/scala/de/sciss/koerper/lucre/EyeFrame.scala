package de.sciss.koerper.lucre

import de.sciss.koerper.lucre.impl.{EyeFrameImpl => Impl}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Workspace

object EyeFrame {
  def apply[S <: Sys[S]](eye: Eye[S], undecorated: Boolean = false)
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): EyeFrame[S] =
    Impl(eye, undecorated = undecorated)
}
trait EyeFrame[S <: Sys[S]] extends Window[S] {
  def eyeView: EyeView[S]


  def fullscreen(implicit tx: S#Tx): Boolean

  def fullscreen_=(value: Boolean)(implicit tx: S#Tx): Unit

  def run(implicit tx: S#Tx): Boolean

  def run_=(value: Boolean)(implicit tx: S#Tx): Unit
}