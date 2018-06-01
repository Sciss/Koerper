package de.sciss.koerper.lucre

import de.sciss.lucre.event.impl.ConstObjImpl
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

object SphereGNG extends Obj.Type {
  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, SphereGNG[S]] =
    anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, SphereGNG[S]] {
    def tpe: Obj.Type = SphereGNG
  }

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val cookie  = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie, expected 3 found $cookie")
    val id = tx.readId(in, access)
    new Impl[S](id)
  }

  final val typeId = 0x50000

//  override def init(): Unit = super.init()

  def apply[S <: Sys[S]](implicit tx: S#Tx): SphereGNG[S] = {
    val id = tx.newId()
    new Impl[S](id)
  }

  private final class Impl[S <: Sys[S]](val id: S#Id)
    extends SphereGNG[S] with ConstObjImpl[S, Any] {

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      new Impl[Out](txOut.newId())
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = ()

    protected def writeData(out: DataOutput): Unit = ()

    override def tpe: Obj.Type = SphereGNG
  }

//  /** An update is a sequence of changes */
//  sealed trait Update[S <: Sys[S]] {
//    def sphere: SphereGNG[S]
//  }
//
//  final case class TableUpdated[S <: Sys[S]](sphere: SphereGNG[S]) extends Update[S]

  /** Value of type `AudioCue.Obj` */
  final val attrTable = "table"

  /** Value of type `StringObj` */
  final val attrOscTargetHost = "osc-target-host"

  /** Value of type `IntObj` */
  final val attrOscTargetPort = "osc-target-port"

  /** Value of type `StringObj` */
  final val attrOscTransport = "osc-transport"

  /** Value of type `StringObj` */
  final val attrOscLocalHost = "osc-local-host"

  /** Value of type `IntObj` */
  final val attrOscLocalPort = "osc-local-port"

  /** Value of type `DoubleObj` */
  final val attrGngEpsilon = "gng-epsilon"

  /** Value of type `DoubleObj` */
  final val attrGngEpsilon2 = "gng-epsilon2"

  /** Value of type `DoubleObj` */
  final val attrGngBeta = "gng-beta"

  /** Value of type `DoubleObj` */
  final val attrGngAlpha = "gng-alpha"

  /** Value of type `DoubleObj` */
  final val attrGngLambda = "gng-lambda"

  /** Value of type `DoubleObj` */
  final val attrGngUtility = "gng-utility"

  // seed

  /** Value of type `IntObj` */
  final val attrGngMaxNodes = "gng-max-nodes"

  /** Value of type `IntObj` */
  final val attrGngMaxEdgeAge = "gng-max-edge-age"

  /** Value of type `IntObj` */
  final val attrGngMaxNeighbors = "gng-max-neighbors"

  /** This excludes `attrTable` */
  val configAttr: Set[String] = Set(
    attrGngEpsilon    , attrGngEpsilon2 , attrGngBeta     , attrGngAlpha      ,
    attrGngLambda     , attrGngUtility  , attrGngMaxNodes , attrGngMaxEdgeAge ,
    attrGngMaxNeighbors
  )

  /** This excludes `attrTable` */
  val oscAttr: Set[String] = Set(
    attrOscTargetHost , attrOscTargetPort , attrOscTransport, attrOscLocalHost, attrOscLocalPort
  )
}
/** A parameters are modelled by the attribute map. */
trait SphereGNG[S <: Sys[S]] extends Obj[S] /* with Publisher[S, SphereGNG.Update[S]] */ {
  // def table: AudioCue.Obj.Var[S]
}
