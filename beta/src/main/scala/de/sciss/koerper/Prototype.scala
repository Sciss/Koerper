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

import scala.concurrent.stm.{Ref, TMap, TxnExecutor}
import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{BorderPanel, Component, Dimension, FlowPanel, Graphics2D, GridPanel, Label, Slider, Swing, ToggleButton}

object Prototype {
  def any2stringadd: Nothing = throw new NotImplementedError()

  def main(args: Array[String]): Unit = {
    run()
  }

  val IpLaptop      = "192.168.0.77"
  val IpPi          = "192.168.0.13"
  val OscPortHH     = 57112
  val NumSounds     = 2187
  val VisualTrajLen = 10
  val MaxNumTraj    = 24
  val Dimensions    = 6

  val isPi: Boolean = sys.props("user.name") == "pi"
  val IpHH: String  = if (isPi) IpPi else IpLaptop

  type Coord = Array[Float]

//  private final class TrajPt(id: Int, coord: Array[Float])

  final case class Traj(cId: Int, pt: Vector[Coord])

//  private final class Frame {
//    var traj = Map.empty[Int, Traj]
//  }

  type Frame = Map[Int, Traj]

  class Views(aural: AuralSystem, rcv: osc.Channel) {
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

    private val dims = (0 until Dimensions).combinations(2).map { case Seq(hDim, vDim) =>
      new Cut(hDim, vDim)
    }

    private val panelCenter = new GridPanel(3, 5 /* 4, 7 */) {
      contents ++= dims
    }

    private def atomic[A](body: Txn => A): A =
      TxnExecutor.defaultAtomic { itx =>
        body(Txn.wrap(itx))
      }

    private val lbInfo = new Label

    private val ggDumpServer = new ToggleButton("scsynth OSC") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          val mode = if (selected) osc.Dump.Text else osc.Dump.Off
          atomic { implicit tx =>
            aural.serverOption.foreach { s =>
              s.peer.dumpOSC(mode)
            }
          }
      }
    }

    private val ggDumpInput = new ToggleButton("input OSC") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          val mode = if (selected) osc.Dump.Text else osc.Dump.Off
          rcv.dump(mode)
      }
    }

    private val ggVolume: Slider = new Slider {
      min = 0
      max = 100

      import numbers.Implicits._

      def get: Double = {
        val n = value.linLin(min, max, -60, 12)
        if (n == -60) 0 else n.dbAmp
      }

      def set(gain: Double): Unit = {
        val n = if (gain == 0.0) -60 else gain.linLin(-60, 12, min, max).clip(min, max).round.toInt
        value = n
      }

      set(1.2.ampDb)

      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          val gain = get
          soundOpt.foreach { sound =>
            atomic { implicit tx =>
              sound.master.set("amp" -> gain)
            }
          }
      }
    }

    private val panelBottom = new FlowPanel(lbInfo, ggDumpServer, ggDumpInput, ggVolume)

    new swing.Frame {
      title = "Beta Test"
      contents = new BorderPanel {
        add(panelCenter, BorderPanel.Position.Center)
        add(panelBottom, BorderPanel.Position.South )
      }
      pack().centerOnScreen()
      open()
    }

    def update(frame: Frame): Unit = {
//      println(frame.valuesIterator.map(_.pt.size).mkString(", "))
      current = frame
      lbInfo.text = s"${frame.size} trajectories"
      panelCenter.repaint()
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
    private final class Elem(val id: Int, val syn: Synth, val timbre: Int, val offset: Double, val dur: Double)

    private[this] val soundMap  = TMap.empty[Int, Elem]
    private[this] var _master: Synth = _

    def master: Synth = _master

    def init()(implicit tx: Txn): this.type = {

      val gM = SynthGraph {
        import de.sciss.synth.Ops.stringToControl
        import de.sciss.synth.ugen._
        val count   = "count".kr(0.0).max(1)
        val amp0    = "amp".kr(1.2).clip(0, 4) * count.sqrt.reciprocal
        val amp     = Lag.ar(amp0, 1.0)
        val in      = LeakDC.ar(In.ar(0, 3))
        val sig     = Limiter.ar(in * amp, level = 0.8)
        ReplaceOut.ar(0, sig)
      }

      _master = Synth.play(gM, nameHint = Some("master"))(target = s.defaultGroup, addAction = addAfter,
        args = Nil)

      this
    }

    private[this] val g = SynthGraph {
      import de.sciss.synth.Ops.stringToControl
      import de.sciss.synth.ugen._
      val buf     = "buf".kr
      val dur     = "dur".kr
      val atk     = "atk".kr
      val rls     = "rls".kr

      val posX0   = "x".kr
      val posY0   = "y".kr
      val posX    = Lag.ar(posX0, 1.0)
      val posY    = Lag.ar(posY0, 1.0)
      val posRad  = posY atan2 posX
      val pos     = posRad / math.Pi
      val amp0    = "amp".kr
      val amp     = Lag.ar(amp0, 1.0)

//      val play    = PlayBuf.ar(numChannels = 1, buf = buf, loop = 0)
      val play    = DiskIn.ar(numChannels = 1, buf = buf, loop = 0)
      val env     = Env.linen(attack = atk, sustain = dur - (atk + rls), release = rls)
      val eg      = EnvGen.ar(env, levelScale = amp, doneAction = freeSelf)
      val sig     = play * eg
      // width = 1.0 means we have gaps (silences)
      val pan     = PanAz.ar(numChannels = 3, in = sig, pos = pos, width = 1.0)
      Out.ar(0, pan)
    }

    private[this] val playCount = Ref(0)

    private def set(e: Elem, c: Coord)(implicit tx: Txn): Unit = {
      import numbers.Implicits._
      val amp   = c(3).linLin(-1, +1, 0, 1).clip(0, 1)
      val x     = c(4).clip(-1, +1)
      val y     = c(5).clip(-1, +1)
      val args: List[ControlSet] = List[ControlSet](
        "x"   -> x,
        "y"   -> y,
        "amp" -> amp
      )
      e.syn.set(args: _*)
    }

    private def play(e: Elem, coord: Coord)(implicit tx: Txn): Unit = {
      import TxnLike.peer
      val info        = soundInfo(e.timbre)
      import numbers.Implicits._
      val numFrames   = (info.numFrames * e.dur.clip(0.1, 1.0)).toInt
      val startFrame  = ((info.numFrames - numFrames) * e.offset.clip(0.0, 1.0)).toInt
      val dur         = numFrames / 44100.0
      val atk         = if (startFrame == 0) 0.0 else 0.002
      val rls         = if (numFrames  == info.numFrames) 0.0 else math.min(dur - atk, 1.0)
      val buf         = Buffer.diskIn(s)(path = info.f.path, startFrame = startFrame)
      val dep         = buf :: Nil
      playCount += 1
      _master.set("count" -> playCount())
      val args: List[ControlSet] = List[ControlSet](
        "buf" -> buf.id,
        "dur" -> dur,
        "atk" -> atk,
        "rls" -> rls
      )
//      val syn = Synth.play(g)(target = s.defaultGroup, addAction = addToHead, args = args, dependencies = dep)
      val syn = e.syn
      syn.play(target = s.defaultGroup, addAction = addToHead, args = args, dependencies = dep)
      set(e, coord)
      syn.onEndTxn { implicit tx =>
        buf.dispose()
        playCount -= 1
        _master.set("count" -> playCount())
        soundMap.remove(e.id)
      }
    }

    def update(frame: Frame)(implicit tx: Txn): Unit = {
      frame.foreach { case (id, traj) =>
        traj.pt.lastOption.foreach { coord =>
          updateWith(id, coord)
        }
      }
    }

    private def updateWith(id: Int, coord: Coord)(implicit tx: Txn): Unit = {
      import TxnLike.peer
      soundMap.get(id).fold[Unit] {
//        val timbre  = (math.random() * NumSounds).toInt
        import numbers.Implicits._
        val timbre  = coord(0).linLin(-1, +1, 0, NumSounds - 1).round.clip(0, NumSounds - 1) // XXX TODO
        val offset  = coord(4 /* 6 */).linLin(-0.9, +0.9, 0, 1).clip(0, 1)
        val dur     = coord(5 /* 7 */).linLin(-0.9, +0.9, 0, 1).clip(0, 1)
        val syn     = Synth(s, g, nameHint = Some("atom"))
        val e = new Elem(id = id, syn = syn, timbre = timbre, offset = offset, dur = dur)
        play(e, coord)
        soundMap.put(id, e)

      } { e =>
        set(e, coord)
      }
    }
  }


  @volatile
  private var soundOpt      = Option.empty[Sound]

  def run(): Unit = {
    SoundProcesses.init()
    mkSoundInfo()

    implicit val aural: AuralSystem = AuralSystem()

    val oscCfg  = osc.UDP.Config()
    oscCfg.localSocketAddress = new InetSocketAddress(IpHH, OscPortHH)
    val rcv     = osc.UDP.Receiver(oscCfg)
    //    rcv.dump()

    lazy val views: Views = new Views(aural, rcv)

    Swing.onEDT {
      views
    }

    val sCfg = Server.Config()
    sCfg.outputBusChannels  = 3
    sCfg.inputBusChannels   = 0
    sCfg.deviceName         = Some("Koerper")
    val cCfg = Client.Config()

    type S = InMemory
    implicit val system: S = InMemory()

    rcv.connect()

    var frameBuilder  = Map.empty[Int, Traj]
    var cyclicIds     = Set(0 until MaxNumTraj: _*)
    var idMap         = Map.empty[Int, Int]

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
          c0: Float, c1: Float, c2: Float, c3: Float, c4: Float, c5: Float /*, c6: Float, c7: Float */) =>

          val t0Opt  = frameBuilder.get(id)
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
