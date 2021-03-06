/*
 *  Session.scala
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
package session

import de.sciss.file._
import de.sciss.fscape.lucre.FScape
import de.sciss.koerper.Koerper.auxDir
import de.sciss.koerper.lucre.{OscNode, SphereGNG}
import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, LongObj, StringObj}
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.osc
import de.sciss.synth.proc
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.MacroImplicits._
import de.sciss.synth.proc.{Action, Durable, Ensemble, Folder, Proc, SoundProcesses, Workspace}

object SessionMini {
  type S = Durable

  final val NameInitStop    = "stop"        // Action
  final val NameRun         = "run"         // BooleanObj: keep iterating
  final val NameFolderUS    = "us"          // Folder: ultra-sound
  final val NameFolderPD    = "pd"          // Folder: probability-distribution
  final val NameLocBase     = "base"        // ArtifactLocation
  final val NameSphere      = "sphere"      // SphereGNG
  final val NameAudio       = "audio"       // Ensemble
  final val NameOsc         = "osc"         // OscNode
  final val NameIterations  = "iterations"  // OscNode

  final val NoPi  = true

  def main(args: Array[String]): Unit = {
    SoundProcesses.init()
    FScape        .init()
    Koerper       .init()

    val ws = build()
    ws.close()
    sys.exit()
  }

  def WorkspaceDir: File = auxDir / "koerper-mini.mllt"

  def build(): Workspace[S] = {
    if (WorkspaceDir.exists()) {
      import sys.process._
      val backup = WorkspaceDir.replaceName("koerper-mini_OLD.mllt")
      if (backup.exists()) Seq("rm", "-r", backup.path).!
      Seq("mv", WorkspaceDir.path, backup.path).!
    }
    val ds = BerkeleyDB     .factory(WorkspaceDir)
    val ws = Workspace.Durable.empty(WorkspaceDir, ds)

    ws.cursor.step { implicit tx =>
      val folder    = ws.root
      val listB     = List.newBuilder[Obj[S]]

      val runB      = BooleanObj.newVar[S](true)
      runB.name     = NameRun
      listB += runB
      val locBase   = ArtifactLocation.newVar[S](Koerper.auxDir)
      locBase.name  = NameLocBase
      listB += locBase

      val actStop   = mkStopEnsembleAction()
      listB += actStop

      val osc   = OscNode[S]
      osc.name  = NameOsc
      val aO    = osc.attr

      val isMacMini = Koerper.auxDir.path.contains("Documents")
      val localIp = if (isMacMini) {
        Koerper.IpMacMini
      } else {
        Koerper.IpHH
      }
      aO.put(OscNode.attrLocalHost  , StringObj .newVar(localIp))
      aO.put(OscNode.attrLocalPort  , IntObj    .newVar(Koerper.OscPortMini       ))
      aO.put(OscNode.attrTargetHost , StringObj .newVar(Koerper.IpRaspiVideo      ))
      aO.put(OscNode.attrTargetPort , IntObj    .newVar(Koerper.OscPortRaspiVideo ))
      val oscRcv = mkOscReceive()
      aO.put(OscNode.attrReceive, oscRcv)
      listB += osc

//      val folderUS  = Folder[S]
//      folderUS.name = NameFolderUS
//      listB += folderUS
      val (recChunkA, recChunkEns) = RecordAudioChunk.mkObjects()
      listB += recChunkA
      listB += recChunkEns
      val constQ    = ConstQConfig.mkObj[S](ConstQConfig())
      listB += constQ
      val renderPD  = RenderProbabilityDistribution.mkObjects(constQ, locBase, osc, noPi = NoPi)
      listB ++= renderPD
//      val folderPD  = Folder[S]
//      folderPD.name = NameFolderPD
//      listB += folderPD
      val sphere    = mkGNG()
      listB += sphere

      val pAudio    = mkUltrasoundPlay()
      val fAudio    = Folder[S]
      fAudio.addLast(pAudio)
      fAudio.addLast(recChunkEns)
      val ensAudio  = Ensemble[S](fAudio, offset = LongObj.newConst(0L), playing = BooleanObj.newVar(true))
      ensAudio.name = NameAudio
      listB += ensAudio

      val count     = IntObj.newVar[S](0)
      count.name    = NameIterations
      listB += count

      if (NoPi) {
        val eyeObj = SessionPi.mkEye(useOsc = false)
        listB ++= eyeObj
      }

      listB.result().foreach(folder.addLast)
    }

    ws
  }

  def mkOscReceive()(implicit tx: S#Tx): Action[S] = {
    val a = proc.Action.apply[S] { u =>
      u.value match {
        case osc.Message("/shutdown") =>
          tx.afterCommit {
            import sys.process._
            println("SHUTDOWN")
            Seq("sudo", "shutdown", "now").run()
          }

        case osc.Message("/reboot"  ) =>
          tx.afterCommit {
            import sys.process._
            println("REBOOT")
            Seq("sudo", "reboot", "now").run()
          }

        case other =>
          println(s"Warning: Ignoring unsupported message $other")
      }
    }
    a.name = "osc-receive"
    a
  }

  def mkUltrasoundPlay()(implicit tx: S#Tx): Proc[S] = {
    val p = Proc[S]
    p.name = "us-play"
    import de.sciss.synth._
    import ugen._
    p.setGraph {
      val sig = SinOsc.ar(40000)
      Out.ar(0, Seq.fill(5)(sig))
    }
    p
  }

  def mkGNG()(implicit tx: S#Tx): SphereGNG[S] = {
    val sphere = SphereGNG[S]
    val a = sphere.attr

//    /* Value of type `AudioCue.Obj` */
//    a.put(SphereGNG.attrTable, ...)
    a.put(SphereGNG.attrOscTargetHost   , StringObj .newVar(Koerper.IpDavid))
    a.put(SphereGNG.attrOscTargetPort   , IntObj    .newVar(Koerper.OscPortDavid))
    a.put(SphereGNG.attrOscTransport    , StringObj .newVar(osc.UDP.name))
    // cheesy test; we can't poll the IP because the machine
    // might be on the wifi using DHCP.
    val isMacMini = Koerper.auxDir.path.contains("Documents")
    val localIp = if (isMacMini) {
      Koerper.IpMacMini
    } else {
      Koerper.IpHH
    }
    a.put(SphereGNG.attrOscLocalHost, StringObj .newVar(localIp))

//    a.put(SphereGNG.attrOscLocalPort, IntObj    .newVar(Koerper.OscPortDavid))
    a.put(SphereGNG.attrGngEpsilon      , DoubleObj .newVar(0.1))
    a.put(SphereGNG.attrGngEpsilon2     , DoubleObj .newVar(0.05))  // 0.001
    a.put(SphereGNG.attrGngBeta         , DoubleObj .newVar(0.001)) // 0.0005
    a.put(SphereGNG.attrGngAlpha        , DoubleObj .newVar(0.5))
    a.put(SphereGNG.attrGngLambda       , DoubleObj .newVar(1.0/50))
    a.put(SphereGNG.attrGngUtility      , DoubleObj .newVar(15.0))  // 20.0
    a.put(SphereGNG.attrGngMaxNodes     , IntObj    .newVar(125))
    a.put(SphereGNG.attrGngMaxEdgeAge   , IntObj    .newVar(100))   // 1000
    a.put(SphereGNG.attrGngMaxNeighbors , IntObj    .newVar(10))
    a.put(SphereGNG.attrGngThrottle     , IntObj    .newVar(50))
    a.put(SphereGNG.attrGngMinEnergy    , DoubleObj .newVar( 20000.0)) // 28000.0
    a.put(SphereGNG.attrGngMaxEnergy    , DoubleObj .newVar(110000.0)) // 140000.0

    sphere.name = NameSphere
    sphere
  }

  def mkStopEnsembleAction()(implicit tx: S#Tx): Action[S] = {
    val a = proc.Action.apply[S] { u =>
      val ens = u.root.![Ensemble]("rec-audio-chunk")
      ens.stop()
    }
    a.name = NameInitStop
    a
  }
}
