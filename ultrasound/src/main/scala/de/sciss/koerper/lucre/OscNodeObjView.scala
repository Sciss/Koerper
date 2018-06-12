/*
 *  OscNodeObjView.scala
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

import de.sciss.desktop
import de.sciss.desktop.OptionPane
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.ListObjViewImpl.NonEditable
import de.sciss.mellite.gui.impl.{ListObjViewImpl, ObjViewImpl}
import de.sciss.mellite.gui.{ListObjView, ObjView}
import de.sciss.synth.proc.Workspace
import javax.swing.Icon

object OscNodeObjView extends ListObjView.Factory {
  type E[~ <: stm.Sys[~]] = OscNode[~]
  val icon      : Icon      = ObjViewImpl.raphaelIcon(raphael.Shapes.Ethernet)
  val prefix    : String    = "OscNode"
  def humanName : String    = "OSC Node"
  def tpe       : Obj.Type  = OscNode
  def category  : String    = ObjView.categMisc

  def hasMakeDialog = true

  private[this] lazy val _init: Unit = ListObjView.addFactory(this)

  def init(): Unit = {
    _init
  }

  def mkListView[S <: Sys[S]](obj: OscNode[S])(implicit tx: S#Tx): OscNodeObjView[S] with ListObjView[S] =
    new Impl(tx.newHandle(obj)).initAttrs(obj)

  final case class Config[S <: stm.Sys[S]](name: String)

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (ok: Config[S] => Unit)
                                 (implicit cursor: stm.Cursor[S]): Unit = {
    val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
      messageType = OptionPane.Message.Question, initial = prefix)
    opt.title = s"New $prefix"
    val res = opt.show(window)
    res.foreach { name =>
      ok(Config(name))
    }
  }

  def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
    val obj = OscNode[S]
    obj :: Nil
  }

  final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, OscNode[S]])
    extends OscNodeObjView[S]
      with ListObjView[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.EmptyRenderer[S]
      with NonEditable[S]
      /* with NonViewable[S] */ {

    override def obj(implicit tx: S#Tx) = objH()

    type E[~ <: stm.Sys[~]] = OscNode[~]

    def factory: ObjView.Factory = OscNodeObjView

    def isViewable = true

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val _obj      = objH()
      val frame     = OscNodeFrame(_obj)
      Some(frame)
    }
  }
}
trait OscNodeObjView[S <: stm.Sys[S]] extends ObjView[S] {
  override def objH: stm.Source[S#Tx , OscNode[S]]
  override def obj(implicit tx: S#Tx): OscNode[S]
}