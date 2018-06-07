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
import de.sciss.koerper.lucre.{Eye, OscNode}
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{DoubleObj, IntObj, LongObj, StringObj}
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.osc
import de.sciss.synth.{io, proc}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.MacroImplicits._
import de.sciss.synth.proc.{Action, AudioCue, Durable, SoundProcesses, Workspace}

object SessionPi {
  type S = Durable

  final val NameLocBase   = "base"  // ArtifactLocation
  final val NameEye       = "eye"   // SphereGNG
  final val NameOsc       = "osc"   // OscNode

  def main(args: Array[String]): Unit = {
    SoundProcesses.init()
    Koerper.init()

    val ws = build()
    ws.close()
    sys.exit()
  }

  def WorkspaceDir: File = auxDir / "koerper-pi.mllt"

  def build(): Workspace[S] = {
    if (WorkspaceDir.exists()) {
      import sys.process._
      val backup = WorkspaceDir.replaceName("koerper-pi_OLD.mllt")
      Seq("rm", "-r", backup.path).!
      Seq("mv", WorkspaceDir.path, backup.path).!
    }
    val ds = BerkeleyDB     .factory(WorkspaceDir)
    val ws = Workspace.Durable.empty(WorkspaceDir, ds)

    ws.cursor.step { implicit tx =>
      val folder    = ws.root
      val listB     = List.newBuilder[Obj[S]]

      val locBase   = ArtifactLocation.newVar[S](Koerper.auxDir)
      locBase.name  = NameLocBase
      listB += locBase
      val eye       = mkEye()
      listB ++= eye
      listB.result().foreach(folder.addLast)
    }

    ws
  }

  def mkEye()(implicit tx: S#Tx): List[Obj[S]] = {
    val eye   = Eye[S]
    eye.name  = NameEye
//    val aE    = eye.attr

    val osc   = OscNode[S]
    osc.name  = NameOsc
    val aO    = osc.attr

    val isPi = Koerper.auxDir.path.contains("Documents")
    if (isPi) {
      aO.put(OscNode.attrLocalHost, StringObj.newVar(Koerper.IpRaspiVideo))
    }

    aO.put(OscNode.attrLocalPort, IntObj.newVar(Koerper.OscPortRaspiVideo))
//    aO.put(OscNode.attrTargetHost , ...)
//    aO.put(OscNode.attrTargetPort , ...)
    val aRcv = mkOscReceive()
    aO.put(OscNode.attrReceive, aRcv)

    eye :: osc :: Nil
  }

  def mkOscReceive()(implicit tx: S#Tx): Action[S] = {
    val a = proc.Action.apply[S] { u =>
      u.value match {
        case osc.Message("/pd", path: String) =>
          println(s"PD PATH $path")
          val locBase = u.root.![ArtifactLocation]("base")
          val eye     = u.root.![de.sciss.koerper.lucre.Eye]("eye")
          val f       = new java.io.File(path)
          val art     = Artifact[S](locBase, f)
          val spec    = io.AudioFile.readSpec(f)
          val cue     = AudioCue.Obj[S](art, spec, offset = LongObj.newConst(0L), gain = DoubleObj.newConst(1.0))
          cue.name    = f.getName
          eye.attr.put("table", cue)

        case osc.Message("/shutdown") =>
          tx.afterCommit {
            import sys.process._
            println("SHUTDOWN")
//            Seq("sudo", "shutdown", "now").run()
          }

        case osc.Message("/reboot"  ) =>
          tx.afterCommit {
            import sys.process._
            println("REBOOT")
//            Seq("sudo", "reboot", "now").run()
          }

        case other =>
          println(s"Warning: Ignoring unsupported message $other")
      }
    }
    a.name = "osc-receive"
    a
  }
}
