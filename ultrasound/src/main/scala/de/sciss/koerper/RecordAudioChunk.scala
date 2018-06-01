/*
 *  RecordAudioChunk.scala
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

import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, DoubleVector}
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.MacroImplicits._
import de.sciss.synth.proc.{AudioCue, Ensemble, Folder, Proc}

object RecordAudioChunk {
  final val KeyOut    = "out"

  final val ValueName = "rec-audio-chunk"

  def mkObjects[S <: Sys[S]]()(implicit tx: S#Tx): List[Obj[S]] = {

    import de.sciss.synth.proc.graph.Ops._
    import de.sciss.synth.proc.graph._
    import de.sciss.synth.ugen.{DiskOut => _, _}

    val p = Proc[S]
    p.setGraph {
      val inChans = "in-channels".kr(0 until 5)
      val dur     = "dur".kr(30.0)
      val in      = PhysicalIn.ar(inChans)
      val sig     = in
      DiskOut.ar("out", sig)
      val done    = Done.kr(Line.kr(0, 0, dur = dur))
      StopSelf(done)
      Action  (done, "done")
    }
    val pa = p.attr
    pa.put("in-channels", DoubleVector.newVar((0 until Koerper.numChannels).iterator.map(_.toDouble).toVector))
    pa.put("dur", DoubleObj.newVar(30.0))

    val aPrep = mkPrepareAction()
    val aDone = mkDoneAction   ()
    pa.put("done", aDone)

    p.name = "proc"

    val f     = Folder[S]
//    f.addLast(aPrep)
    f.addLast(p)
    val pl    = BooleanObj.newVar[S](false)
    val ens   = Ensemble(f, offset = 0L, playing = pl)
    ens.name  = ValueName

    aPrep :: ens :: Nil // List(aPrep, p, aDone)
  }

  private def mkPrepareAction[S <: Sys[S]]()(implicit tx: S#Tx): Obj[S] = {
    val a = proc.Action.apply[S] { u =>
      val locBase = u.root ![ArtifactLocation] "base"
      val ens     = u.root ![Ensemble] "rec-audio-chunk"
      ens.stop()
      val pRec    = ens ![Proc] "proc"
      // N.B. we have a race condition when using AIFF: the done action may see
      // the file before the header is flushed, thus reading numFrames == 0.
      // If we use IRCAM, the header does not carry numFrames information.
      val fmt     = new java.text.SimpleDateFormat("'us-'yyMMdd_HHmmss'.irc'", java.util.Locale.US)
      val name    = fmt.format(new java.util.Date)
      val child   = new java.io.File("us", name).getPath
      val art     = Artifact(locBase, Artifact.Child(child))
      pRec.attr.put("out", art)
      ens.play()
    }

    a.name = "rec-prepare"
    a
  }

  private def mkDoneAction[S <: Sys[S]]()(implicit tx: S#Tx): Obj[S] = {
    val a = proc.Action.apply[S] { u =>
      val folder  = u.root ![Folder] "us"
      val ens     = u.root ![Ensemble] "rec-audio-chunk"
      ens.stop()
      val pRec    = ens ![Proc] "proc"
      val art     = pRec.attr ![Artifact] "out"
      val artVal  = art.value
      val spec    = de.sciss.synth.io.AudioFile.readSpec(artVal)
      val cue     = AudioCue.Obj(art, spec, offset = 0L, gain = 1.0)
      cue.name    = artVal.getName
      folder.addLast(cue)
    }

    a.name = "rec-done"
    a
  }
}
