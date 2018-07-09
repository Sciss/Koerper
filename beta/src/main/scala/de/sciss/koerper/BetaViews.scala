/*
 *  BetaViews.scala
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

import de.sciss.koerper.KoerperBeta.{Config, Dimensions, Frame, atomic, soundOpt}
import de.sciss.synth.proc.AuralSystem
import de.sciss.{numbers, osc}

import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{BorderPanel, Component, Dimension, FlowPanel, Graphics2D, GridPanel, Label, Slider, ToggleButton}

class BetaViews(config: Config, aural: AuralSystem, rcv: osc.Channel) {
  private val colors = Array.tabulate(config.maxNumTraj) { i =>
    val hue = i.toFloat / config.maxNumTraj
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

    set(config.masterGain.ampDb)

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
