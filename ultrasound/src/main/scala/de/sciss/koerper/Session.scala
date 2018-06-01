/*
 *  Session.scala
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

import de.sciss.fscape.lucre.FScape
import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.proc.{Durable, Folder, SoundProcesses, Workspace}
import de.sciss.synth.proc.Implicits._

object Session {
  type S = Durable

  final val NameFolderUS  = "us"  // ultra-sound
  final val NameFolderPD  = "pd"  // probability-distribution
  final val NameLocBase   = "base"

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
      listB.result().foreach(folder.addLast)
    }

    ws
  }
}
