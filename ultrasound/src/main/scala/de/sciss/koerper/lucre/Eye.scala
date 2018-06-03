/*
 *  Eye.scala
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

import de.sciss.lucre.event.impl.ConstObjImpl
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object Eye extends Obj.Type {
  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Eye[S]] =
    anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, Eye[S]] {
    def tpe: Obj.Type = Eye
  }

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val cookie  = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie, expected 3 found $cookie")
    val id = tx.readId(in, access)
    new Impl[S](id)
  }

  final val typeId = 0x50001

  def apply[S <: Sys[S]](implicit tx: S#Tx): Eye[S] = {
    val id = tx.newId()
    new Impl[S](id)
  }

  private final class Impl[S <: Sys[S]](val id: S#Id)
    extends Eye[S] with ConstObjImpl[S, Any] {

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      new Impl[Out](txOut.newId())
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = ()

    protected def writeData(out: DataOutput): Unit = ()

    override def tpe: Obj.Type = Eye
  }

  /** Value of type `AudioCue.Obj` */
  final val attrTable = "table"

  /** In seconds. Value of type `DoubleObj` */
  final val attrFadeTime = "fade-time"
}
trait Eye[S <: Sys[S]] extends Obj[S]
