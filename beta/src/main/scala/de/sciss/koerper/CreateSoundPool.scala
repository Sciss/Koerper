/*
 *  CreateSoundPool.scala
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
import de.sciss.synth.io.AudioFile

import scala.collection.mutable
import scala.util.control.NonFatal

object CreateSoundPool {
  def mkInputIterator(): Iterator[File] = {
    val url = this.getClass.getResource("/InputSounds.txt")
    io.Source.fromURL(url, "UTF-8").getLines().map(file)
  }

  def main(args: Array[String]): Unit = {
    validateList()
  }

  def validateList(): Unit = {
    val minFrames = 18 * 44100L
    val map       = mutable.Map.empty[Long, List[File]]
    mkInputIterator().foreach { f =>
      try {
        val spec  = AudioFile.readSpec(f)
        if (spec.numChannels < 0) {
          println(s"NO CHANNELS: $f")
        } else if (spec.numFrames < minFrames) {
          println(s"TOO SHORT: $f")
        } else {
          val others = map.getOrElse(spec.numFrames, Nil)
          if (others.exists { that =>
            val a = AudioFile.openRead(f)
            try {
              val b = AudioFile.openRead(that)
              try {
                val ba = a.buffer(44100)
                a.read(ba)
                val bb = b.buffer(44100)
                b.read(bb)
                ba(0) sameElements bb(0)

              } finally {
                 b.cleanUp()
              }
            } finally {
              a.cleanUp()
            }
          }) {
            println(s"DUPLICATE: $f")
          } else {
            map.put(spec.numFrames, f :: others)
          }
        }
      } catch {
        case NonFatal(_) => println(s"INVALID FILE: $f")
      }
    }
  }
}
