/*
 *  BetaSound.scala
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

import de.sciss.file._
import de.sciss.koerper.AdHocMap.Key
import de.sciss.koerper.KoerperBeta.{Config, Coord, Frame, NumSounds, soundInfo}
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.IntSpace.TwoDim
import de.sciss.lucre.geom.{IntDistanceMeasure2D, IntPoint2D}
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.{Buffer, Group, Server, Synth, Sys, Txn}
import de.sciss.numbers
import de.sciss.synth.{ControlSet, SynthGraph, addAfter, addToHead, freeSelf}

import scala.concurrent.stm.{Ref, TMap}

final class BetaSound[S <: Sys[S]](s: Server, config: Config, timbreMap: SkipOctree[S, TwoDim, Key]) {
  def any2stringadd: Nothing = throw new NotImplementedError()

  private final class Elem(val id: Int, val syn: Synth, val timbre: Int, val offset: Double, val dur: Double)

  private[this] val bgSoundMap = TMap.empty[Int, Elem]
  private[this] val fgSoundMap = TMap.empty[Int, Elem]

  private[this] var _bgGroup    : Group = _
  private[this] var _fgGroup    : Group = _
  private[this] var _regulator  : Synth = _
  private[this] var _master     : Synth = _

  def master: Synth = _master

  def init()(implicit tx: Txn): this.type = {
    _bgGroup = Group.play(s.defaultGroup, addToHead)
    _fgGroup = Group.play(_bgGroup      , addAfter )

    val gReg = SynthGraph {
      import de.sciss.synth.Ops.stringToControl
      import de.sciss.synth.ugen._
      val count   = "count".kr(0.0).max(1)
      val amp0    = count.sqrt.reciprocal
      val amp     = Lag.kr(amp0, 1.0) // Lag.ar(amp0, 1.0)
      val in      = In.ar(0, 3)
      val sig     = in * amp
      ReplaceOut.ar(0, sig)
    }

    val gM = SynthGraph {
      import de.sciss.synth.Ops.stringToControl
      import de.sciss.synth.ugen._
      val amp0    = "amp".kr(1.2).clip(0, 4)
      val amp     = amp0 // Lag.kr(amp0, 1.0) // Lag.ar(amp0, 1.0)
      val in      = LeakDC.ar(In.ar(0, 3))
      val sig     = Limiter.ar(in * amp, level = config.limiter)
      ReplaceOut.ar(0, sig)
    }

    _regulator = Synth.play(gReg, nameHint = Some("regulator"))(target = _bgGroup, addAction = addAfter,
      args = Nil)
    _master = Synth.play(gM, nameHint = Some("master"))(target = _fgGroup, addAction = addAfter,
      args = Nil)

    this
  }

  private[this] val gBg = SynthGraph {
    import de.sciss.synth.Ops.stringToControl
    import de.sciss.synth.ugen._
    val buf     = "buf".kr
    val dur     = "dur".kr
    val atk     = "atk".kr
    val rls     = "rls".kr

    val posX0   = "x".kr
    val posY0   = "y".kr
    val posX    = Lag.kr(posX0, 1.0) // Lag.ar(posX0, 1.0)
    val posY    = Lag.kr(posY0, 1.0) // Lag.ar(posY0, 1.0)
    val posRad  = posY atan2 posX
    val pos     = posRad / math.Pi
    val amp0    = "amp".kr
    val amp     = Lag.kr(amp0, 1.0) // Lag.ar(amp0, 1.0)

    //      val play    = PlayBuf.ar(numChannels = 1, buf = buf, loop = 0)
    val play    = DiskIn.ar(numChannels = 1, buf = buf, loop = 0)
    val env     = Env.linen(attack = atk, sustain = dur - (atk + rls), release = rls)
    val eg      = EnvGen.ar(env, levelScale = amp, doneAction = freeSelf)
    val sig     = play * eg
    // width = 1.0 means we have gaps (silences)
    val pan     = PanAz.ar(numChannels = 3, in = sig, pos = pos, width = 1.0)
    Out.ar(0, pan)
  }

  private[this] val gFg = SynthGraph {
    import de.sciss.synth.Ops.stringToControl
    import de.sciss.synth.ugen._
    val buf     = "buf" .kr
    val bufDly  = "dly" .kr
    val dur     = "dur" .kr
    val ch      = "chan".kr
    val amp0    = "amp" .kr
    val amp     = amp0
    val atk     = 1.0
    val rls     = 1.0
    val play    = DiskIn.ar(numChannels = 1, buf = buf, loop = 0) * amp
    //    val sig     = DelayN.ar(play, maxDelayTime = atk, delayTime = atk)
    val sig     = BufDelayN.ar(bufDly, play, delayTime = atk)
    val env     = Env.linen(attack = atk, sustain = dur - (atk + rls), release = rls)
    val eg      = EnvGen.kr(env, doneAction = freeSelf) // EnvGen.ar(env, doneAction = freeSelf)
    XOut.ar(ch, sig, xfade = eg)
  }

  private[this] val playCount = Ref(0)

  private def set(e: Elem, coord: Coord)(implicit tx: Txn): Unit = {
    import numbers.Implicits._
    val amp   = coord(3).linLin(-1, +1, 0, 1).clip(0, 1)
    val x     = coord(4).clip(-1, +1)
    val y     = coord(5).clip(-1, +1)
    val args: List[ControlSet] = List[ControlSet](
      "x"   -> x,
      "y"   -> y,
      "amp" -> amp
    )
    e.syn.set(args: _*)
  }

  private def playBg(e: Elem, coord: Coord)(implicit tx: Txn): Unit = {
    import TxnLike.peer
    val info        = soundInfo(e.timbre)
    import numbers.Implicits._
    val numFrames   = (info.numFrames * e.dur.clip(0.1, 1.0)).toInt
    val startFrame  = ((info.numFrames - numFrames) * e.offset.clip(0.0, 1.0)).toInt
    val durSec      = numFrames / 44100.0
    val atk         = if (startFrame == 0) 0.0 else 0.002
    val rls         = if (numFrames  == info.numFrames) 0.0 else math.min(durSec - atk, 1.0)
    val buf         = Buffer.diskIn(s)(path = info.f.path, startFrame = startFrame)
    val dep         = buf :: Nil
    playCount += 1
    _regulator.set("count" -> playCount())
    val args: List[ControlSet] = List[ControlSet](
      "buf" -> buf.id,
      "dur" -> durSec,
      "atk" -> atk,
      "rls" -> rls
    )
    //      val syn = Synth.play(g)(target = s.defaultGroup, addAction = addToHead, args = args, dependencies = dep)
    val syn = e.syn
    syn.play(target = _bgGroup, addAction = addToHead, args = args, dependencies = dep)
    set(e, coord)
    syn.onEndTxn { implicit tx =>
      buf.dispose()
      playCount -= 1
      _regulator.set("count" -> playCount())
      bgSoundMap.remove(e.id)
    }
  }

  def update(frame: Frame)(implicit tx: S#Tx): Unit = {
    import TxnLike.peer
    frame.foreach { case (id, traj) =>
      val didSync = (traj.age == config.minSyncLen) && {
        val ok0   = !fgSoundMap.contains(0)
        val ok1   = !fgSoundMap.contains(1)
        val ok2   = !fgSoundMap.contains(2)
        val ch    = if (ok0) 0 else if (ok1) 1 else if (ok2) 2 else -1
        val ok    = ok0 || ok1 || ok2
        if (ok) {
          println(s"SYNC $id -> $ch")
          playFg(ch = ch, id = id, coord = traj.pt.last)
        }
        ok
      }
      if (!didSync) traj.pt.lastOption.foreach { coord =>
        updateBg(id, coord)
      }
    }
  }

  private def timbreFromCoord(coord: Coord)(implicit tx: S#Tx): Int = {
    import numbers.Implicits._
    val x: Int = coord(0).linLin(-1, +1, 0, 1024).round.clip(-1, 1024)
    val y: Int = coord(1).linLin(-1, +1, 0, 1024).round.clip(-1, 1024)
    val timbre = timbreMap.nearestNeighborOption(IntPoint2D(x, y), IntDistanceMeasure2D.euclideanSq).fold[Int] {
      println("Woops, no neighbour")
      coord(0).linLin(-1, +1, 0, NumSounds - 1).round.clip(0, NumSounds - 1)
    } (_.id)
    timbre
  }

  private def playFg(ch: Int, id: Int, coord: Coord)(implicit tx: S#Tx): Unit = {
    import TxnLike.peer
    import numbers.Implicits._
    val timbre      = timbreFromCoord(coord)
    val offsetNorm  = 0.0
    val durNorm     = 1.0
    val syn         = Synth(s, gFg, nameHint = Some("sync"))
    val e           = new Elem(id = id, syn = syn, timbre = timbre, offset = offsetNorm, dur = durNorm)
    val info        = soundInfo(e.timbre)
    val numFrames   = info.numFrames
    val durSec      = numFrames / 44100.0
    val buf         = Buffer.diskIn(s)(path = info.f.path)
    val bufDly      = Buffer(s)(numFrames = 65536) // must be power of two: 44100
    val dep         = buf :: Nil
    val args: List[ControlSet] = List[ControlSet](
      "buf"   -> buf    .id,
      "dly"   -> bufDly .id,
      "dur"   -> durSec,
      "chan"  -> ch,
      "amp"   -> config.fgGain
    )
    syn.play(target = _fgGroup, addAction = addToHead, args = args, dependencies = dep)
    set(e, coord)
    syn.onEndTxn { implicit tx =>
      buf   .dispose()
      bufDly.dispose()
      fgSoundMap.remove(ch)
    }
    fgSoundMap.put(ch, e)
  }

  private def updateBg(id: Int, coord: Coord)(implicit tx: S#Tx): Unit = {
    import TxnLike.peer
    bgSoundMap.get(id).fold[Unit] {
      import numbers.Implicits._
      val timbre = timbreFromCoord(coord)

      //        val timbre  = (math.random() * NumSounds).toInt
      //        val timbre  = coord(0).linLin(-1, +1, 0, NumSounds - 1).round.clip(0, NumSounds - 1) // XXX TODO
      val offset  = coord(4 /* 6 */).linLin(-0.9, +0.9, 0, 1).clip(0, 1)
      val dur     = coord(5 /* 7 */).linLin(-0.9, +0.9, 0, 1).clip(0, 1)
      val syn     = Synth(s, gBg, nameHint = Some("atom"))
      val e = new Elem(id = id, syn = syn, timbre = timbre, offset = offset, dur = dur)
      playBg(e, coord)
      bgSoundMap.put(id, e)

    } { e =>
      set(e, coord)
    }
  }
}
