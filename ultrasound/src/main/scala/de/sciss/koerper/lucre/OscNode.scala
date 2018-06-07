package de.sciss.koerper.lucre

import java.net.SocketAddress

import de.sciss.koerper.lucre.impl.{OscNodeImpl => Impl}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.osc
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.WorkspaceHandle

object OscNode extends Obj.Type {
  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, OscNode[S]] = Impl.serializer[S]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  final val typeId = 0x50002

  def apply[S <: Sys[S]](implicit tx: S#Tx): OscNode[S] = Impl[S]

  /** Expected type is `IntObj` */
  final val attrLocalPort   = "local-port"
  /** Expected type is `IntObj` */
  final val attrTargetPort  = "target-port"
  /** Expected type is `StringObj` */
  final val attrLocalHost   = "local-host"
  /** Expected type is `StringObj` */
  final val attrTargetHost  = "target-host"
  /** Expected type is `StringObj` */
  final val attrTransport   = "transport"
  /** Expected type is `Action` */
  final val attrReceive     = "receive"
}
trait OscNode[S <: Sys[S]] extends Obj[S] {
  def ! (p: osc.Packet)(implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: WorkspaceHandle[S]): Boolean

  def send (p: osc.Packet, target: SocketAddress)(implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: WorkspaceHandle[S]): Boolean

//  /** Just a shortcut for accessing `attrReceive`. */
//  def receive(implicit tx: S#Tx): Option[Action[S]]
//
//  /** Just a shortcut for accessing `attrReceive`. */
//  def receive_=(value: Option[Action[S]])(implicit tx: S#Tx): Unit
}