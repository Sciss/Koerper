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
import de.sciss.mellite.gui.ActionOpenWorkspace

import scala.swing.Swing

object Koerper {
  final val numChannels = 5

  val baseDir: File = {
    val tmp = file("/data") / "projects" / "Koerper"
    if (tmp.isDirectory) tmp else userHome / "Documents" / "projects" / "Koerper"
  }

  val auxDir: File = baseDir / "aux"

  val IpDavid           = "192.168.0.21"
  val IpMacMini         = "192.168.0.20"
  val IpRaspiVideo      = "192.168.0.27"
  val OscPortDavid      = 7771
  val OscPortRaspiVideo = 57111
  val OscSocketDavid    = new InetSocketAddress(IpDavid, OscPortDavid)
  val JackClientName    = "Koerper"

  def VoronoiCoordFile(ch: Int): File = {
    require (ch >= 0 && ch < Koerper.numChannels)
    Koerper.auxDir / s"voronoi_coord-${ch+1}.aif"
  }

  def SphereCoordFile(ch: Int): File = {
    require (ch >= 0 && ch < Koerper.numChannels)
    Koerper.auxDir / s"sphere_coord-${ch+1}.aif"
  }

  def WorkspaceDir: File = auxDir / "koerper.mllt"

  final case class Config(workspaceOpt: Option[File] = None, startObjects: List[String] = Nil)

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Körper-Ultrasound") {
      opt[File]('w', "workspace")
        .text("Directory of workspace (.mllt) to automatically open")
        .action { (f, c) => c.copy(workspaceOpt = Some(f)) }

      opt[Seq[String]]('s', "start")
        .text("List of names of start objects in workspace's root directory")
        .action { (v, c) => c.copy(startObjects = v.toList) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def run(config: Config): Unit = {
    Mellite.main(new Array(0))
    Swing.onEDT {
      de.sciss.koerper.lucre.SphereGNG        .init()
      de.sciss.koerper.lucre.Eye              .init()
      de.sciss.koerper.lucre.OscNode          .init()
      de.sciss.koerper.lucre.SphereGNGObjView .init()
      de.sciss.koerper.lucre.EyeObjView       .init()
      de.sciss.koerper.lucre.OscNodeObjView   .init()
    }
    config.workspaceOpt.foreach { wsF =>
      ActionOpenWorkspace.perform(wsF)
    }
  }

  def raspiDisableEnergySaving(): Unit = {
    import sys.process._
    Seq("xset", "s", "off").!
    Seq("xset", "-dpms").!
  }
}
