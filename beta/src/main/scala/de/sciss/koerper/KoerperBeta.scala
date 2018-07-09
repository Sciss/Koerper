/*
 *  KoerperBeta.scala
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

import java.awt.geom.Path2D
import java.awt.{Color, RenderingHints}
import java.net.InetSocketAddress

import de.sciss.file._
import de.sciss.koerper.AdHocMap.Key
import de.sciss.kollflitz.Vec
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.{IntDistanceMeasure2D, IntPoint2D}
import de.sciss.lucre.geom.IntSpace.TwoDim
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.{Buffer, InMemory, Server, Synth, Sys, Txn}
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{AuralSystem, SoundProcesses}
import de.sciss.synth.{Client, ControlSet, SynthGraph, addAfter, addToHead, freeSelf}
import de.sciss.{numbers, osc}

import scala.concurrent.stm.{Ref, TMap, TxnExecutor}
import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{BorderPanel, Component, Dimension, FlowPanel, Graphics2D, GridPanel, Label, Slider, Swing, ToggleButton}

object KoerperBeta {
  def any2stringadd: Nothing = throw new NotImplementedError()

  final case class Config(minNumTraj: Int = 2, minSyncLen: Int = 200, maxNumTraj: Int = 24, oscPort: Int = 57112,
                          gui: Boolean = false, masterGain: Double = 1.2, limiter: Double = 0.8)

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Körper-Beta") {
      opt[Int]('m', "min-traj")
        .text(s"Minimum number of trajectories for background layer (default: ${default.minNumTraj})")
        .validate { v => if (v >= 0) success else failure("Must be >= 0") }
        .action { (v, c) => c.copy(minNumTraj = v) }

      opt[Int]('l', "min-len")
        .text(s"Minimum trajectory length of synchronized layer (default: ${default.minSyncLen})")
        .validate { v => if (v > 0) success else failure("Must be > 0") }
        .action { (v, c) => c.copy(minSyncLen = v) }

      opt[Int]('x', "max-traj")
        .text(s"Maximum number of traced trajectories (default: ${default.maxNumTraj})")
        .validate { v => if (v > 0) success else failure("Must be > 0") }
        .action { (v, c) => c.copy(maxNumTraj = v) }

      opt[Int]('p', "port")
        .text(s"OSC reception port (default: ${default.oscPort})")
        .action { (v, c) => c.copy(oscPort = v) }

      opt[Double]('g', "gain")
        .text(s"Master gain, linear (default: ${default.masterGain})")
        .validate { v => if (v > 0) success else failure("Must be > 0") }
        .action { (v, c) => c.copy(masterGain = v) }

      opt[Double]("limiter")
        .text(s"Limiter ceiling, linear (default: ${default.limiter})")
        .validate { v => if (v > 0 && v <= 1) success else failure("Must be > 0 and <= 1") }
        .action { (v, c) => c.copy(limiter = v) }

      opt[Unit]('g', "gui")
        .text("Open GUI")
        .action { (_, c) => c.copy(gui = true) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  val IpLaptop      = "192.168.0.77"
  val IpPi          = "192.168.0.13"
  val NumSounds     = 2187
  val VisualTrajLen = 10
  val Dimensions    = 6

  val isPi: Boolean = sys.props("user.name") == "pi"
  val IpHH: String  = if (isPi) IpPi else IpLaptop

  type Coord = Array[Float]

  //  private final class TrajPt(id: Int, coord: Array[Float])

  final case class Traj(cId: Int, pt: Vector[Coord], age: Int)

  //  private final class Frame {
  //    var traj = Map.empty[Int, Traj]
  //  }

  type Frame = Map[Int, Traj]

  final case class SoundInfo(f: File, numFrames: Int)

  @volatile
  var soundInfo: Vec[SoundInfo] = Vector.empty

  def atomic[A](body: Txn => A): A =
    TxnExecutor.defaultAtomic { itx =>
      body(Txn.wrap(itx))
    }

  def mkSoundInfo(): Unit = {
    soundInfo = (0 until NumSounds).map { id =>
      val f     = CreateSoundPool.formatTemplate(CreateSoundPool.tempPhaseOut, id + 1)
      val spec  = AudioFile.readSpec(f)
      SoundInfo(f, spec.numFrames.toInt)
    }
  }

  type S = InMemory

  @volatile
  var soundOpt = Option.empty[BetaSound[S]]

  def run(config: Config): Unit = {
    SoundProcesses.init()
    mkSoundInfo()

    implicit val aural: AuralSystem = AuralSystem()

    val oscCfg  = osc.UDP.Config()
    oscCfg.localSocketAddress = new InetSocketAddress(IpHH, config.oscPort)
    val rcv     = osc.UDP.Receiver(oscCfg)
    //    rcv.dump()

    lazy val views: Option[BetaViews] = if (config.gui) Some(new BetaViews(config, aural, rcv)) else None

    Swing.onEDT {
      views
    }

    val sCfg = Server.Config()
    sCfg.outputBusChannels  = 3
    sCfg.inputBusChannels   = 0
    sCfg.deviceName         = Some("Koerper")
    val cCfg = Client.Config()

    implicit val system: S = InMemory()

    val (_, timbreMap: SkipOctree[S, TwoDim, Key]) = system.step { implicit tx =>
      AdHocMap.mkMap[S]()
    }

    rcv.connect()

    var lastFrame     = Map.empty[Int, Traj]
    var nextFrame     = Map.empty[Int, Traj]
    val cyclicIds0    = Set(0 until config.maxNumTraj: _*)
    var cyclicIds     = cyclicIds0
    var idMap         = Map.empty[Int, Int]

    rcv.action = { (p, _) =>
      p match {
        case osc.Message("/f_new") =>
          val currentFrame  = nextFrame
          lastFrame         = currentFrame
          nextFrame         = Map.empty
          val usedCyclic    = currentFrame.valuesIterator.map(_.cId).toSet
          cyclicIds         = cyclicIds0 -- usedCyclic
          soundOpt.foreach { sound =>
            val f1: Map[Int, Traj] = if (currentFrame.size >= config.minNumTraj) currentFrame else Map.empty
            system.step { implicit tx =>
              sound.update(f1)
            }
          }
          if (config.gui) Swing.onEDT {
            views.foreach(_.update(currentFrame))
          }

        case osc.Message("/t_set", id: Int,
          c0: Float, c1: Float, c2: Float, c3: Float, c4: Float, c5: Float /*, c6: Float, c7: Float */) =>

          val t0Opt  = nextFrame.get(id).orElse(lastFrame.get(id))
          t0Opt.foreach { t0 =>
            val t1 = if (t0.pt.size < VisualTrajLen) t0 else t0.copy(pt = t0.pt.tail)
            val c: Coord = {
              val a = new Array[Float](Dimensions /* 8 */)
              a(0) = c0
              a(1) = c1
              a(2) = c2
              a(3) = c3
              a(4) = c4
              a(5) = c5
              //              a(6) = c6
              //              a(7) = c7
              a
            }
            val t2 = t1.copy(pt = t1.pt :+ c, age = t1.age + 1)
            nextFrame += id -> t2
          }

        case osc.Message("/t_new", id: Int) =>
          cyclicIds.headOption.foreach { cId =>
            cyclicIds -= cId
            idMap.get(id).foreach { cId => cyclicIds += cId }
            idMap += id -> cId
            val t = Traj(cId = cId, pt = Vector.empty, age = 0)
            nextFrame += id -> t
            //            println(s"t_new $id / $cId; total ${frameBuilder.size}")
          }

        case osc.Message("/t_end", id: Int) =>
          lastFrame -= id
          nextFrame -= id
          idMap.get(id).foreach { cId =>
            cyclicIds += cId
            idMap -= id
          }
        //          println(s"t_end ; total ${frameBuilder.size}")

        case osc.Message("/gain", amp: Float) =>
          soundOpt.foreach { sound =>
            atomic { implicit tx =>
              sound.master.set("amp" -> amp)
            }
          }

        case osc.Message("/shutdown") =>
//          tx.afterCommit {
            import sys.process._
            println("SHUTDOWN")
            Seq("sudo", "shutdown", "now").run()
//          }

        case osc.Message("/reboot"  ) =>
//          tx.afterCommit {
            import sys.process._
            println("REBOOT")
            Seq("sudo", "reboot", "now").run()
//          }

        case _ =>
          println(s"Warning: dropping unknown OSC packet $p")
      }
    }

    system.step { implicit tx =>
      aural.addClient(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit tx: Txn): Unit = {
          val sound = new BetaSound[S](s, config, timbreMap).init()
          soundOpt = Some(sound)
        }

        def auralStopped()(implicit tx: Txn): Unit = {
          soundOpt = None
        }
      })
      aural.start(sCfg, cCfg)
    }

    /*

r: [ "/f_new" ]
r: [ "/t_set", 1, -0.22083525, 0.25855842, 0.28556648, 0.26348463, -0.4846659, 0.97511214, -0.42286474, 0.8963914 ]
r: [ "/t_set", 2, -0.08910237, 0.637218, 0.85439825, 0.37193075, -0.55620813, -0.9352911, -0.2650562, 0.9854168 ]
r: [ "/t_set", 3, -0.40243223, -0.26929086, -0.58777857, -0.7783102, 0.4199592, 0.7265548, -0.4676051, 0.9731216 ]
r: [ "/f_new" ]
r: [ "/t_set", 1, -0.2181432, 0.2624448, 0.2841074, 0.26422346, -0.49181047, 0.97437227, -0.43170637, 0.8988561 ]
r: [ "/t_set", 2, -0.07902449, 0.6361511, 0.8559544, 0.37114963, -0.5562467, -0.93326676, -0.26351032, 0.98619443 ]
r: [ "/t_set", 3, -0.4031034, -0.26697153, -0.58777857, -0.77824706, 0.419746, 0.72368526, -0.47364712, 0.9813359 ]
r: [ "/f_new" ]

     */
  }
}
