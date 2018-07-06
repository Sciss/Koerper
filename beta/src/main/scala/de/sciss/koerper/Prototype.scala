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

import java.awt.{Color, RenderingHints}
import java.awt.geom.Path2D
import java.net.InetSocketAddress

import de.sciss.lucre.synth.{InMemory, Server, Txn}
import de.sciss.osc
import de.sciss.synth.Client
import de.sciss.synth.proc.{AuralSystem, SoundProcesses}

import scala.swing.{Component, Dimension, Graphics2D, GridPanel, Swing}

object Prototype {
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

    new swing.Frame {
      title = "Beta Test"
      contents = panel
      pack().centerOnScreen()
      open()
    }

    def update(frame: Frame): Unit = {
      println(frame.valuesIterator.map(_.pt.size).mkString(", "))
      current = frame
      panel.repaint()
    }
  }

  def run(): Unit = {
    SoundProcesses.init()

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

    rcv.action = { (p, _) =>
      p match {
        case osc.Message("/f_new") =>
          val currentFrame  = frameBuilder
          // frameBuilder      = Map.empty
          Swing.onEDT {
            views.update(currentFrame)
          }

        case osc.Message("/t_set", id: Int,
          c0: Float, c1: Float, c2: Float, c3: Float, c4: Float, c5: Float, c6: Float, c7: Float) =>

          val t0Opt  = frameBuilder.get(id)
          t0Opt.foreach { t0 =>
            val t1 = if (t0.pt.size < VisualTrajLen) t0 else t0.copy(pt = t0.pt.tail)
            val c: Coord = Array(c0, c1, c2, c3, c4, c5, c6, c7)
            val t2 = t1.copy(pt = t1.pt :+ c)
            frameBuilder += id -> t2
          }

        case osc.Message("/t_new", id: Int) =>
          cyclicIds.headOption.foreach { cId =>
            idMap.get(id).foreach { cId => cyclicIds += cId }
            idMap += id -> cId
            val t = Traj(cId = cId, pt = Vector.empty)
            frameBuilder += id -> t
            println(s"t_new $id / $cId; total ${frameBuilder.size}")
          }

        case osc.Message("/t_end", id: Int) =>
          frameBuilder -= id
          idMap.get(id).foreach { cId =>
            cyclicIds += cId
            idMap -= id
          }
          println(s"t_end; total ${frameBuilder.size}")

        case _ =>
          println(s"Warning: dropping unknown OSC packet $p")
      }
    }

    system.step { implicit tx =>
      aural.addClient(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit tx: Txn): Unit = ()

        def auralStopped()(implicit tx: Txn): Unit = ()
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
