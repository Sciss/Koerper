/*
 *  Koerper.scala
 *  (Körper)
 *
 *  Copyright (c) 2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.koerper

import java.net.{InetSocketAddress, PortUnreachableException}

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.synth.{InMemory, Server, Txn}
import de.sciss.mellite.Mellite
import de.sciss.neuralgas.sphere.SphereGNG
import de.sciss.osc
import de.sciss.synth.Client
import de.sciss.synth.proc.{AuralSystem, SoundProcesses}

import scala.concurrent.{Future, Promise}
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
//    run(Config())
    Mellite.main(args)
    Swing.onEDT {
      de.sciss.koerper.lucre.SphereGNG.init()
      de.sciss.koerper.lucre.SphereGNGObjView.init()
    }
  }

  def run(config: Config): Unit = {
    /*

      - (SP)boot audio system
      - establish OSC connection
      - create GNG and load state (send GNG state to David)
      - set up visual process
      - start GNG ongoing process
      - (SP) start audio recording loop:
         - (SP)record all channels
         - (FSc) transform to voronoi files
         - inject / "fade" (?) into PD

     */

    implicit val system: S = InMemory()

    val futAural  = bootAudioSystem()
    val oscT      = mkOscTransmitter()
    val gng       = startGNG(oscT)

    import SoundProcesses.executionContext

    futAural.foreach { _ =>
      println("Aural system ready.")
    }
  }

  type S = InMemory

  def bootAudioSystem()(implicit cursor: stm.Cursor[S]): Future[Unit] = {
    SoundProcesses.init()

    val p  = Promise[Unit]()
    val as = AuralSystem()

    val sCfg = Server.Config()
    sCfg.inputBusChannels   = Koerper.numChannels   // ultra-sound receivers
    sCfg.outputBusChannels  = Koerper.numChannels   // ultra-sound transmitters
    sCfg.deviceName         = Some(JackClientName)
    sCfg.sampleRate         = 96000
    sCfg.transport          = osc.TCP
    val cCfg = Client.Config()
    cCfg.executionContext   = SoundProcesses.executionContext

    cursor.step { implicit tx =>
      as.addClient(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit tx: Txn): Unit =
          tx.afterCommit(p.trySuccess(()))

        def auralStopped()(implicit tx: Txn): Unit = ()
      })
      as.start(sCfg, cCfg)
    }

    p.future
  }

  type Trns = osc.UDP.Transmitter.Undirected

  def trySend(t: Trns, p: osc.Packet): Boolean =
    try {
      t.send(p, OscSocketDavid)
      true
    } catch {
      case _: PortUnreachableException => false
    }

  def mkOscTransmitter(): Trns = {
    val oscCfg    = osc.UDP.Config()
    oscCfg.codec  = osc.PacketCodec().doublePrecision()
    val oscT      = osc.UDP.Transmitter(oscCfg)
    oscT.connect()
    oscT.dump()

    var attemptsLeft = 120  // in 60 seconds
    while({
      !trySend(oscT, osc.Message("/reset")) && attemptsLeft > 0
    }) {
      attemptsLeft -= 1
      Thread.sleep(500)
    }

    if (attemptsLeft == 0) Console.err.println("WARNING: Could not send /reset OSC message!")

    oscT
  }

  def startGNG(oscT: Trns): SphereGNG = {
    null // ???
  }
}
