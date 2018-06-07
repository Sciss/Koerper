package de.sciss.koerper.lucre
package impl

import java.net.{InetAddress, InetSocketAddress, SocketAddress}

import de.sciss.lucre.event.impl.ConstObjImpl
import de.sciss.lucre.expr.{IntObj, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Disposable, Elem, IdentifierMap, NoSys, Obj, Sys, TxnLike}
import de.sciss.osc
import de.sciss.osc.Packet
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc.{Action, WorkspaceHandle}

import scala.concurrent.stm.{TMap, TSet}
import scala.util.Try

object OscNodeImpl {
  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, OscNode[S]] =
    anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, OscNode[S]] {
    def tpe: Obj.Type = OscNode
  }

  private[this] val map = TMap.empty[WorkspaceHandle[_], WorkspaceMap[_]]

  private final class WorkspaceMap[S <: Sys[S]](stateMap: IdentifierMap[S#Id, S#Tx, Stateful[S]])
                                               (implicit cursor: stm.Cursor[S], workspace: WorkspaceHandle[S])
    extends Disposable[S#Tx] {

    private[this] val stateSet = TSet.empty[Stateful[S]]

    def getOrMk(n: OscNode[S])(implicit tx: S#Tx): Stateful[S] = {
      import TxnLike.peer
      val id = n.id
      stateMap.getOrElse(id, {
        val a = n.attr
        val localPort   = a.$[IntObj    ](OscNode.attrLocalPort ).fold(0  )(_.value)
        val targetPort  = a.$[IntObj    ](OscNode.attrTargetPort).fold(0  )(_.value)
        val localHost   = a.$[StringObj ](OscNode.attrLocalHost ).fold("" )(_.value)
        val targetHost  = a.$[StringObj ](OscNode.attrTargetHost).fold("" )(_.value)
        val targetAddr  = if (targetPort == 0) None else Try {
          val addr = if (targetHost == "") InetAddress.getLoopbackAddress else InetAddress.getByName(targetHost)
          new InetSocketAddress(addr, targetPort)
        } .toOption

        val receiveH = a.$[Action](OscNode.attrReceive).map(tx.newHandle(_))

        val cfg   = osc.UDP.Config()
        if (localHost != "") cfg.localSocketAddress = new InetSocketAddress(localHost, localPort)
        else cfg.localPort = localPort

        cfg.codec = osc.PacketCodec().arrays().doubles().booleans().packetsAsBlobs()
//        tx.afterCommit {
//          val trns  = osc.UDP.Transmitter(cfg)
//          val rcv   = osc.UDP.Receiver(trns.channel, cfg)
//          trns.connect()
//          rcv .connect()
//        }

        val res = new StatefulDirectedUDPImpl[S](cfg, defaultTarget = targetAddr, nodeH = tx.newHandle(n),
          receiveH = receiveH).init()
        stateMap.put(id, res)
        stateSet += res
        res
      })
    }

    def remove(n: OscNode[S], s: Stateful[S])(implicit tx: S#Tx): Unit = {
      import TxnLike.peer
      stateMap.remove(n.id)
      stateSet.remove(s)
    }

    def isEmpty(implicit tx: S#Tx): Boolean = {
      import TxnLike.peer
      stateSet.isEmpty
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      import TxnLike.peer
      stateMap.dispose()
      stateSet.foreach(_.dispose())
      stateSet.clear()
    }
  }

  def stateful[S <: Sys[S]](n: OscNode[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                           workspace: WorkspaceHandle[S]): Stateful[S] = {
    import TxnLike.peer
    val res = map.get(workspace).getOrElse {
      val objMap  = tx.newInMemoryIdMap[Stateful[S]]
      val res0    = new WorkspaceMap[S](objMap)
      workspace.addDependent(res0)
      map.put(workspace, res0)
      res0
    }
    val wMap = res.asInstanceOf[WorkspaceMap[S]]
    wMap.getOrMk(n)
  }

  private def remove[S <: Sys[S]](n: OscNode[S], s: Stateful[S])
                                 (implicit tx: S#Tx, workspace: WorkspaceHandle[S]): Unit = {
    import TxnLike.peer
    map.get(workspace).foreach { wMap0 =>
      val wMap = wMap0.asInstanceOf[WorkspaceMap[S]]
      wMap.remove(n, s)
      if (wMap.isEmpty) {
        map.remove(workspace)
        wMap.dispose()
      }
    }
  }

  def apply[S <: Sys[S]](implicit tx: S#Tx): OscNode[S] = {
    val id = tx.newId()
    new Impl[S](id)
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = {
    val cookie  = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie, expected 3 found $cookie")
    val id = tx.readId(in, access)
    new Impl[S](id)
  }

  private final class StatefulDirectedUDPImpl[S <: Sys[S]](cfg: osc.UDP.Config, defaultTarget: Option[SocketAddress],
                                                           nodeH: stm.Source[S#Tx, OscNode[S]],
                                                           receiveH: Option[stm.Source[S#Tx, Action[S]]])
                                                          (implicit cursor: stm.Cursor[S], workspace: WorkspaceHandle[S])
    extends Stateful[S] {

    private[this] var trns: osc.UDP.Transmitter.Undirected = _
    private[this] var rcv : osc.UDP.Receiver   .Undirected = _

    def node(implicit tx: S#Tx): OscNode[S] = nodeH()

    def init()(implicit tx: S#Tx): this.type = {
      tx.afterCommit {
        trns  = osc.UDP.Transmitter(cfg)
        rcv   = osc.UDP.Receiver(trns.channel, cfg)
        trns.connect()
        rcv .connect()
        receiveH.foreach { rh =>
          rcv.action = { (p, addr) =>
            cursor.step { implicit tx =>
              val a = rh()
              val n = nodeH()
              val u = Action.Universe(self = a, workspace = workspace, invoker = Some(n), value = p)
              a.execute(u)
            }
          }
        }
      }
      this
    }

    def ! (p: Packet)/* (implicit tx: S#Tx) */: Boolean = defaultTarget.exists { addr =>
      send(p, addr)
    }

    def send(p: Packet, target: SocketAddress)/* (implicit tx: S#Tx) */: Boolean =
      /* tx.afterCommit */ {
        val t = trns
        (t != null) && {
          t.send(p, target)
          true
        }
      }

    def dump(mode: osc.Dump): Unit = {
      val t = trns
      if (t != null) t.dump(mode)
      val r = rcv
      if (r != null) r.dump(mode)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      OscNodeImpl.remove(node, this)
      tx.afterCommit {
        if (trns != null) trns.close()
        if (rcv  != null) rcv .close()
      }
    }
  }

  trait Stateful[S <: Sys[S]] extends Disposable[S#Tx] {
    def ! (p: Packet)/* (implicit tx: S#Tx) */: Boolean

    def send(p: Packet, target: SocketAddress)/* (implicit tx: S#Tx) */: Boolean

    def dump(mode: osc.Dump): Unit
  }

  private final class Impl[S <: Sys[S]](val id: S#Id)
    extends OscNode[S] with ConstObjImpl[S, Any] {

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      new Impl[Out](txOut.newId())
    }

//    protected def disposeData()(implicit tx: S#Tx): Unit = ()

    protected def writeData(out: DataOutput): Unit = ()

    def tpe: Obj.Type = OscNode

    ////////

    def ! (p: Packet)(implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: WorkspaceHandle[S]): Boolean = {
      val state = stateful(this)
      tx.afterCommit {
        state ! p
      }
      true
    }

    def send(p: Packet, target: SocketAddress)(implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: WorkspaceHandle[S]): Boolean = {
      val state = stateful(this)
      tx.afterCommit {
        state.send(p, target)
      }
      true
    }
  }
}
