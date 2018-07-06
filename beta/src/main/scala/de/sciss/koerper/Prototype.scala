/*
 *  Prototype.scala
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
import de.sciss.kollflitz.Vec
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.{Buffer, InMemory, Server, Synth, Txn}
import de.sciss.{numbers, osc}
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{AuralSystem, SoundProcesses}
import de.sciss.synth.{Client, ControlSet, SynthGraph, addAfter, addToHead, freeSelf}

import scala.concurrent.stm.{Ref, TMap}
import scala.swing.{BorderPanel, Component, Dimension, Graphics2D, GridPanel, Label, Swing}

object Prototype {
  def any2stringadd: Nothing = throw new NotImplementedError()

  def main(args: Array[String]): Unit = {
    run()
  }

  val IpHH          = "192.168.0.77"
  val OscPortHH     = 57112
  val NumSounds     = 1093    // XXX TODO currently
  val VisualTrajLen = 10
  val MaxNumTraj    = 24

  type Coord = Array[Float]

//  private final class TrajPt(id: Int, coord: Array[Float])

  final case class Traj(cId: Int, pt: Vector[Coord])

//  private final class Frame {
//    var traj = Map.empty[Int, Traj]
//  }

  type Frame = Map[Int, Traj]

  class Views {
    private val colors = Array.tabulate(MaxNumTraj) { i =>
      val hue = i.toFloat / MaxNumTraj
      Color.getHSBColor(hue, 1f, 1f)
    }

    private[this] var current: Frame = Map.empty

    private class Cut(hDim: Int, vDim: Int) extends Component {
      private[this] val gp = new Path2D.Float

      preferredSize = new Dimension(200, 200)

      override protected def paintComponent(g: Graphics2D): Unit = {
        val p = peer
        val w = p.getWidth
        val h = p.getHeight
        g.setColor(Color.black)
        g.fillRect(0, 0, w, h)
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )
        current.foreach {
          case (_, traj) =>
            val c = colors(traj.cId)
            g.setColor(c)
            gp.reset()
            var move = true
            traj.pt.foreach { coord =>
              val x = (coord(hDim) *  0.5f + 0.5f) * w
              val y = (coord(vDim) * -0.5f + 0.5f) * h
              if (move) {
                move = false
                gp.moveTo(x, y)
              } else {
                gp.lineTo(x, y)
              }
            }
            g.draw(gp)
        }
      }
    }

    private val dims = (0 until 8).combinations(2).map { case Seq(hDim, vDim) =>
      new Cut(hDim, vDim)
    }

    private val panel = new GridPanel(4, 7) {
      contents ++= dims
    }

    private val lbInfo = new Label

    new swing.Frame {
      title = "Beta Test"
      contents = new BorderPanel {
        add(panel, BorderPanel.Position.Center)
        add(lbInfo, BorderPanel.Position.South)
      }
      pack().centerOnScreen()
      open()
    }

    def update(frame: Frame): Unit = {
//      println(frame.valuesIterator.map(_.pt.size).mkString(", "))
      current = frame
      lbInfo.text = s"${frame.size} trajectories"
      panel.repaint()
    }
  }

  final case class SoundInfo(f: File, numFrames: Int)

  private var soundInfo: Vec[SoundInfo] = Vector.empty

  def mkSoundInfo(): Unit = {
    soundInfo = (0 until NumSounds).map { id =>
      val f     = CreateSoundPool.formatTemplate(CreateSoundPool.tempPhaseOut, id + 1)
      val spec  = AudioFile.readSpec(f)
      SoundInfo(f, spec.numFrames.toInt)
    }
  }

  private final class Sound(s: Server) {
    private final class Elem(val id: Int, val timbre: Int, val offset: Double, val dur: Double)

    private[this] val soundMap  = TMap.empty[Int, Elem]
    private[this] var master: Synth = _

    def init()(implicit tx: Txn): this.type = {

      val gM = SynthGraph {
        import de.sciss.synth.Ops.stringToControl
        import de.sciss.synth.ugen._
        val count   = "count".kr(0.0).max(1)
        val amp0    = "amp".kr(1.0).clip(0, 1) * 0.4 * count.sqrt.reciprocal
        val amp     = Lag.ar(amp0, 1.0)
        val in      = LeakDC.ar(In.ar(0, 3))
        val sig     = Limiter.ar(in * amp, level = 0.4)
        ReplaceOut.ar(0, sig)
      }

      master = Synth.play(gM, nameHint = Some("master"))(target = s.defaultGroup, addAction = addAfter,
        args = Nil)

      this
    }

    private[this] val g = SynthGraph {
      import de.sciss.synth.Ops.stringToControl
      import de.sciss.synth.ugen._
      val posRad  = "pos".kr
      val pos     = posRad / math.Pi
      val amp     = "amp".kr
      val buf     = "buf".kr
      val dur     = "dur".kr
      val atk     = "atk".kr
      val rls     = "rls".kr
//      val play    = PlayBuf.ar(numChannels = 1, buf = buf, loop = 0)
      val play    = DiskIn.ar(numChannels = 1, buf = buf, loop = 0)
      val env     = Env.linen(attack = atk, sustain = dur - (atk + rls), release = rls)
      val eg      = EnvGen.ar(env, levelScale = amp, doneAction = freeSelf)
      val sig     = play * eg
      val pan     = PanAz.ar(numChannels = 3, in = sig, pos = pos)
      Out.ar(0, pan)
    }

    private[this] val playCount = Ref(0)

    private def play(e: Elem)(implicit tx: Txn): Unit = {
      import TxnLike.peer
      val info        = soundInfo(e.timbre)
      import numbers.Implicits._
      val numFrames   = (info.numFrames * e.dur.clip(0.1, 1.0)).toInt
      val startFrame  = ((info.numFrames - numFrames) * e.offset.clip(0.0, 1.0)).toInt
      val dur         = numFrames / 44100.0
      val atk         = if (startFrame == 0) 0.0 else 0.002
      val rls         = if (numFrames  == info.numFrames) 0.0 else 1.0
      val buf         = Buffer.diskIn(s)(path = info.f.path, startFrame = startFrame)
      val dep         = buf :: Nil
      val pos         = e.id % 3 * (2 * math.Pi) / 3
      playCount += 1
      master.set("count" -> playCount())
      val args: List[ControlSet] = List[ControlSet](
        "pos" -> pos,
        "amp" -> 1.0,
        "buf" -> buf.id,
        "dur" -> dur,
        "atk" -> atk,
        "rls" -> rls
      )
      val syn = Synth.play(g)(target = s.defaultGroup, addAction = addToHead, args = args, dependencies = dep)
      syn.onEndTxn { implicit tx =>
        buf.dispose()
        playCount -= 1
        master.set("count" -> playCount())
        soundMap.remove(e.id)
      }
    }

    def update(frame: Frame)(implicit tx: Txn): Unit = {
      import TxnLike.peer
      frame.foreach { case (id, _ /* traj */) =>
        soundMap.get(id).fold[Unit] {
          val timbre  = (math.random() * NumSounds).toInt
          val offset  = 0.0
          val dur     = 1.0
          val e = new Elem(id = id, timbre = timbre, offset = offset, dur = dur)
          play(e)
          soundMap.put(id, e)

        } (_ => ())
      }
    }
  }

  def run(): Unit = {
    SoundProcesses.init()
    mkSoundInfo()

    lazy val views: Views = new Views

    Swing.onEDT {
      views
    }

    implicit val aural: AuralSystem = AuralSystem()
    val sCfg = Server.Config()
    sCfg.outputBusChannels  = 3
    sCfg.inputBusChannels   = 0
    sCfg.deviceName         = Some("Koerper")
    val cCfg = Client.Config()

    type S = InMemory
    implicit val system: S = InMemory()

    val oscCfg  = osc.UDP.Config()
    oscCfg.localSocketAddress = new InetSocketAddress(IpHH, OscPortHH)
    val rcv     = osc.UDP.Receiver(oscCfg)
//    rcv.dump()
    rcv.connect()

    var frameBuilder  = Map.empty[Int, Traj]
    var cyclicIds     = Set(0 until MaxNumTraj: _*)
    var idMap         = Map.empty[Int, Int]

    @volatile
    var soundOpt      = Option.empty[Sound]

    rcv.action = { (p, _) =>
      p match {
        case osc.Message("/f_new") =>
          val currentFrame  = frameBuilder
          // frameBuilder      = Map.empty
          soundOpt.foreach { sound =>
            system.step { implicit tx =>
              sound.update(currentFrame)
            }
          }
          Swing.onEDT {
            views.update(currentFrame)
          }

        case osc.Message("/t_set", id: Int,
          c0: Float, c1: Float, c2: Float, c3: Float, c4: Float, c5: Float, c6: Float, c7: Float) =>

          val t0Opt  = frameBuilder.get(id)
          t0Opt.foreach { t0 =>
            val t1 = if (t0.pt.size < VisualTrajLen) t0 else t0.copy(pt = t0.pt.tail)
            val c: Coord = {
              val a = new Array[Float](8)
              a(0) = c0
              a(1) = c1
              a(2) = c2
              a(3) = c3
              a(4) = c4
              a(5) = c5
              a(6) = c6
              a(7) = c7
              a
            }
            val t2 = t1.copy(pt = t1.pt :+ c)
            frameBuilder += id -> t2
          }

        case osc.Message("/t_new", id: Int) =>
          cyclicIds.headOption.foreach { cId =>
            cyclicIds -= cId
            idMap.get(id).foreach { cId => cyclicIds += cId }
            idMap += id -> cId
            val t = Traj(cId = cId, pt = Vector.empty)
            frameBuilder += id -> t
//            println(s"t_new $id / $cId; total ${frameBuilder.size}")
          }

        case osc.Message("/t_end", id: Int) =>
          frameBuilder -= id
          idMap.get(id).foreach { cId =>
            cyclicIds += cId
            idMap -= id
          }
//          println(s"t_end ; total ${frameBuilder.size}")

        case _ =>
          println(s"Warning: dropping unknown OSC packet $p")
      }
    }

    system.step { implicit tx =>
      aural.addClient(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit tx: Txn): Unit = {
          val sound = new Sound(s).init()
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
