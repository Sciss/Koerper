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
import de.sciss.koerper.lucre.SphereGNG
import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj, StringObj}
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.osc
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Durable, Folder, SoundProcesses, Workspace}

object Session {
  type S = Durable

  final val NameRun       = "run"     // BooleanObj: keep iterating
  final val NameFolderUS  = "us"      // Folder: ultra-sound
  final val NameFolderPD  = "pd"      // Folder: probability-distribution
  final val NameLocBase   = "base"    // ArtifactLocation
  final val NameSphere    = "sphere"  // SphereGNG

  def main(args: Array[String]): Unit = {
    SoundProcesses.init()
    FScape        .init()

    val ws = build()
    ws.close()
    sys.exit()
  }

  def build(): Workspace[S] = {
    val ds = BerkeleyDB     .factory(Koerper.WorkspaceDir)
    val ws = Workspace.Durable.empty(Koerper.WorkspaceDir, ds)

    ws.cursor.step { implicit tx =>
      val folder    = ws.root
      val listB     = List.newBuilder[Obj[S]]

      val runB      = BooleanObj.newVar[S](true)
      runB.name     = NameRun
      listB += runB
      val locBase   = ArtifactLocation.newVar[S](Koerper.auxDir)
      locBase.name  = NameLocBase
      listB += locBase
      val folderUS  = Folder[S]
      folderUS.name = NameFolderUS
      listB += folderUS
      val recChunk  = RecordAudioChunk.mkObjects()
      listB ++= recChunk
      val constQ    = ConstQConfig.mkObj[S](ConstQConfig())
      listB += constQ
      val renderPD  = RenderProbabilityDistribution.mkObjects(constQ, locBase)
      listB ++= renderPD
      val folderPD  = Folder[S]
      folderPD.name = NameFolderPD
      listB += folderPD
      val sphere    = mkGNG()
      listB += sphere
      listB.result().foreach(folder.addLast)
    }

    ws
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
    if (isMacMini) {
      a.put(SphereGNG.attrOscLocalHost, StringObj .newVar(Koerper.IpMacMini))
    }
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
//    a.put(SphereGNG.attrGngMinEnergy    , DoubleObj .newVar(...))
//    a.put(SphereGNG.attrGngMaxEnergy    , DoubleObj .newVar(...))

    sphere.name = NameSphere
    sphere
  }
}
