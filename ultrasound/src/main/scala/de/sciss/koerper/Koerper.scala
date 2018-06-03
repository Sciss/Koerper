/*
 *  Koerper.scala
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

package de.sciss.koerper

import java.net.InetSocketAddress

import de.sciss.file._
import de.sciss.mellite.Mellite

import scala.swing.Swing

object Koerper {
  final val numChannels = 5

  val baseDir: File = {
    val tmp = file("/data") / "projects" / "Koerper"
    if (tmp.isDirectory) tmp else userHome / "Documents" / "projects" / "Koerper"
  }

  val auxDir: File = baseDir / "aux"

  val IpDavid         = "192.168.0.21"
  val IpMacMini       = "192.168.0.20"
  val OscPortDavid    = 7771
  val OscSocketDavid  = new InetSocketAddress(IpDavid, OscPortDavid)
  val JackClientName  = "Koerper"

  def VoronoiCoordFile(ch: Int): File = {
    require (ch >= 0 && ch < Koerper.numChannels)
    Koerper.auxDir / s"voronoi_coord-${ch+1}.aif"
  }

  def SphereCoordFile(ch: Int): File = {
    require (ch >= 0 && ch < Koerper.numChannels)
    Koerper.auxDir / s"sphere_coord-${ch+1}.aif"
  }

  def WorkspaceDir: File = auxDir / "koerper.mllt"

  final case class Config()

  def main(args: Array[String]): Unit = {
    Mellite.main(args)
    Swing.onEDT {
      de.sciss.koerper.lucre.SphereGNG        .init()
      de.sciss.koerper.lucre.Eye              .init()
      de.sciss.koerper.lucre.SphereGNGObjView .init()
      de.sciss.koerper.lucre.EyeObjView       .init()
    }
  }

//  def bootAudioSystem[S <: Sys[S]]()(implicit cursor: stm.Cursor[S]): Future[Unit] = {
//    SoundProcesses.init()
//
//    val p  = Promise[Unit]()
//    val as = AuralSystem()
//
//    val sCfg = Server.Config()
//    sCfg.inputBusChannels   = Koerper.numChannels   // ultra-sound receivers
//    sCfg.outputBusChannels  = Koerper.numChannels   // ultra-sound transmitters
//    sCfg.deviceName         = Some(JackClientName)
//    sCfg.sampleRate         = 96000
//    sCfg.transport          = osc.TCP
//    val cCfg = Client.Config()
//    cCfg.executionContext   = SoundProcesses.executionContext
//
//    cursor.step { implicit tx =>
//      as.addClient(new AuralSystem.Client {
//        def auralStarted(s: Server)(implicit tx: Txn): Unit =
//          tx.afterCommit(p.trySuccess(()))
//
//        def auralStopped()(implicit tx: Txn): Unit = ()
//      })
//      as.start(sCfg, cCfg)
//    }
//
//    p.future
//  }
}
