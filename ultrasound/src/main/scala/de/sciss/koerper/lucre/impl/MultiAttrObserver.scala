package de.sciss.koerper.lucre
package impl

import de.sciss.lucre.expr.{DoubleObj, IntObj, StringObj}
import de.sciss.lucre.{event => evt}
import de.sciss.lucre.stm.{Disposable, Obj, Sys, TxnLike}

import scala.concurrent.stm.TMap

trait MultiAttrObserver[S <: Sys[S]] {
  // ---- abstract ----

  private[this] var observer: Disposable[S#Tx] = _

  private[this] val valueObsMap = TMap.empty[String, Disposable[S#Tx]]

  protected def multiAttrKeys: Set[String]

  protected def checkMultiAttrUpdate(map: EvtMap, key: String, value: Obj[S])(implicit tx: S#Tx): Boolean

  protected def multiAttrMap(implicit tx: S#Tx): EvtMap

  // ---- impl ----

  protected final type EvtMap = evt.Map[S, String, Obj]

  protected final def disposeMultiAttrObserver()(implicit tx: S#Tx): Unit =
    observer.dispose()

  protected final def getDouble(key: String, default: Double)(implicit tx: S#Tx, map: EvtMap): Double =
    map.get(key) match {
      case Some(v: DoubleObj[S])  => v.value
      case Some(v: IntObj   [S])  => v.value.toDouble
      case _                      => default
    }

  // accepts doubles as well
  protected final def getIntD(key: String, default: Int)(implicit tx: S#Tx, map: EvtMap): Int =
    map.get(key) match {
      case Some(v: IntObj   [S])  => v.value
      case Some(v: DoubleObj[S])  => v.value.toInt
      case _                      => default
    }

  protected final def getInt(key: String, default: Int)(implicit tx: S#Tx, map: EvtMap): Int =
    map.get(key) match {
      case Some(v: IntObj[S]) => v.value
      case _                  => default
    }

  protected final def getString(key: String, default: String)(implicit tx: S#Tx, map: EvtMap): String =
    map.get(key) match {
      case Some(v: StringObj[S])  => v.value
      case _                      => default
    }

  private def checkMultiAttrUpdate1(map: EvtMap, key: String, value: Obj[S], remove: Boolean)(implicit tx: S#Tx): Unit =
    if (checkMultiAttrUpdate(map, key, value)) {
      mkValueObs(key, value, remove = remove)
    }

  private def mkValueObs(key: String, value: Obj[S], remove: Boolean)(implicit tx: S#Tx): Unit = {
    import TxnLike.peer
    valueObsMap.remove(key).foreach(_.dispose())
    val evtOpt = if (remove) None else value match {
      case vt: IntObj   [S] => Some(vt.changed)
      case vt: DoubleObj[S] => Some(vt.changed)
      case vt: StringObj[S] => Some(vt.changed)
      case _                => None
    }
    evtOpt.foreach { evt =>
      val obs = evt.react { implicit tx => _ =>
        val map = multiAttrMap
        map.get(key).foreach { value1 =>
          checkMultiAttrUpdate1(map, key, value1, remove = false)
        }
      }
      valueObsMap.put(key, obs)
    }
  }

  final protected def initMultiAttr(obj: Obj[S])(implicit tx: S#Tx): Unit = {
    observer = obj.attr.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case Obj.AttrAdded    (key, value)    => checkMultiAttrUpdate1(upd.map, key, value, remove = false)
        case Obj.AttrRemoved  (key, value)    => checkMultiAttrUpdate1(upd.map, key, value, remove = true )
        case Obj.AttrReplaced (key, _, value) => checkMultiAttrUpdate1(upd.map, key, value, remove = false)
        case _ =>
      }
    }

    implicit val map: EvtMap = obj.attr

    val allKeys = multiAttrKeys
    allKeys.foreach { k =>
      map.get(k).foreach { v =>
        mkValueObs(k, v, remove = false)
      }
    }
  }
}
