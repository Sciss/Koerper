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

import de.sciss.desktop.DocumentHandler
import de.sciss.file._
import de.sciss.koerper.lucre.{Eye, EyeFrame, OscNode, OscNodeFrame, SphereGNG, SphereGNGFrame}
import de.sciss.lucre.stm
import de.sciss.lucre.synth.{InMemory, Sys}
import de.sciss.mellite.gui.{ActionOpenWorkspace, EnsembleFrame}
import de.sciss.mellite.{Application, Mellite}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Action, Ensemble, Workspace}

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
  val IpHH              = "192.168.0.77"
  val IpRaspiVideo      = "192.168.0.27"
  val OscPortDavid      = 7771
  val OscPortRaspiVideo = 57111
  val OscPortMini       = 57111
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

  final case class Config(workspaceOpt: Option[File] = None, startObjects: List[String] = Nil,
                          cleanAux: Boolean = false)

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Körper-Ultrasound") {
      opt[File]('w', "workspace")
        .text("Directory of workspace (.mllt) to automatically open")
        .action { (f, c) => c.copy(workspaceOpt = Some(f)) }

      opt[Seq[String]]('s', "start")
        .text("List of names of start objects in workspace's root directory")
        .action { (v, c) => c.copy(startObjects = v.toList) }

      opt[Unit]('c', "clean")
        .text("Clean old aux files")
        .action { (_, c) => c.copy(cleanAux = true) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def init(): Unit = {
    de.sciss.koerper.lucre.SphereGNG        .init()
    de.sciss.koerper.lucre.Eye              .init()
    de.sciss.koerper.lucre.OscNode          .init()
    de.sciss.koerper.lucre.SphereGNGObjView .init()
    de.sciss.koerper.lucre.EyeObjView       .init()
    de.sciss.koerper.lucre.OscNodeObjView   .init()
  }

  private def deleteFiles(files: Seq[File], retain: Int = 20): Unit = {
    val toDelete = files.sorted(File.NameOrdering).dropRight(retain)
    toDelete.foreach(_.delete())
  }

  def run(config: Config): Unit = {
    if (config.cleanAux) {
      val usF = (auxDir / "us").children(_.extL == "irc")
      deleteFiles(usF)
      val pdF = (auxDir / "us").children(_.extL == "aif")
      deleteFiles(pdF)
    }

    Mellite.main(new Array(0))
    Swing.onEDT {
      init()
      config.workspaceOpt.foreach { wsF =>
        if (config.startObjects.nonEmpty) {
          Application.documentHandler.addListener {
            case DocumentHandler.Added(ws) =>
              // XXX TODO -- why Mellite takes longer opening the root window?
              val t = new javax.swing.Timer(4000, { _ =>
                implicit val damn: Workspace[InMemory] = ws.asInstanceOf[Workspace[InMemory]]
                start[InMemory](config.startObjects)
              })
              t.setRepeats(false)
              t.start()
          }
        }
        ActionOpenWorkspace.perform(wsF)
//      if (config.startObjects.nonEmpty) {
//        val docOpt: Option[Workspace[_ <: stm.Sys[_]]] = Application.documentHandler.documents.find(_.folder.contains(wsF))
//        docOpt.fold[Unit] {
//          Console.err.println(s"Warning: could not find opened workspace '${wsF.name}'.")
//        } { ws =>
//          implicit val damn: Workspace[InMemory] = ws.asInstanceOf[Workspace[InMemory]]
//          start[InMemory](config.startObjects)
//        }
      }
    }
  }

  def start[S <: Sys[S]](names: List[String])(implicit workspace: Workspace[S]): Unit = {
    implicit val cursor: stm.Cursor[S] = workspace.cursor
    cursor.step { implicit tx =>
      val r = workspace.root
      names.foreach { name =>
        r./(name).fold[Unit] {
          Console.err.println(s"Warning: start object '$name' not found in root folder.")
        } {
          case obj: Action    [S] => startAction  (obj)
          case obj: Ensemble  [S] => startEnsemble(obj)
          case obj: Eye       [S] => startEye     (obj)
          case obj: OscNode   [S] => startOscNode (obj)
          case obj: SphereGNG [S] => startSphere  (obj)
          case obj =>
            Console.err.println(s"Warning: no idea how to 'start' object '$name' of type ${obj.tpe}.")
        }
      }
    }
  }

  def startAction[S <: Sys[S]](a: Action[S])
                              (implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: Workspace[S]): Unit = {
    val u = Action.Universe(self = a, workspace = workspace)
    a.execute(u)
  }

  def startEnsemble[S <: Sys[S]](ens: Ensemble[S])
                                (implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: Workspace[S]): Unit = {
    val f = EnsembleFrame(ens)
    f.ensembleView.transport.play()
  }

  def startEye[S <: Sys[S]](eye: Eye[S])
                           (implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: Workspace[S]): Unit = {
    val f = EyeFrame(eye, undecorated = true)
    f.fullscreen  = true
    f.run         = true
  }

  def startOscNode[S <: Sys[S]](n: OscNode[S])
                               (implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: Workspace[S]): Unit = {
    OscNodeFrame(n)
  }

  def startSphere[S <: Sys[S]](sph: SphereGNG[S])
                              (implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: Workspace[S]): Unit = {
    val f = SphereGNGFrame(sph)
    f.run = true
  }

//  def raspiDisableEnergySaving(): Unit = {
//    import sys.process._
//    Seq("xset", "s", "off").!
//    Seq("xset", "-dpms").!
//  }
}
