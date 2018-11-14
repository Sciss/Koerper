package de.sciss.koerper

import de.sciss.file._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object MinPhaseTest {
  def main(args: Array[String]): Unit = {
//    val fIn   = file("/data/projects/Tumulus/audio_work/OM1_test_180816c.aif")
//    val fOut  = file("/data/temp/min-phase-test.aif")
    val fIn   = file("/data/projects/Tumulus/data/rec180911_143120.wav")
    val fOut  = file(s"/data/temp/${fIn.base}-min-phase.aif")
    val fut   = CreateSoundPool.mkMinPhase(fIn, fOut)
    Await.result(fut, Duration.Inf)
    sys.exit()
  }
}
