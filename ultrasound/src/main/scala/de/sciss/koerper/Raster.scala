/*
 *  Raster.scala
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

import de.sciss.file._
import de.sciss.fscape.{Graph, graph}
import de.sciss.fscape.stream.Control
import de.sciss.koerper.Calibration.formatTemplate
import de.sciss.koerper.DopplerTest.{Config, analyze, calcNumWin, calcWinStep}
import de.sciss.koerper.Geom._
import de.sciss.koerper.Voronoi.{ScalarOps, voronoiCentersPt3, voronoiCornersPt3, voronoiPolygons}
import de.sciss.kollflitz
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.math._

object Raster {
  def CoordFile(ch: Int): File = {
    require (ch >= 0 && ch < 5)
    Koerper.auxDir / s"voronoi_coord-${ch+1}.aif"
  }

//  private final val CoordCookie = 0x436F6F72  // "Coor"

  /*

  val nRing = (1 until ring).map { i =>
    val f = i.linLin(0, ring, 0.0, math.Pi).sin
    val n = (f * ring + 0.5).toInt
    n
  }

  nRing.sum // 2970204

   */

  def init(): Unit = {

  }

  def fibonacciSphere(num: Int, step: Double = 1.0): Array[Pt3] = {
    val points    = new Array[Pt3](num)
    val offset    = 2.0 / num
    val increment = Pi * (3.0 - sqrt(5.0))

    for (i <- 0 until num) {
      val y     = ((i * offset) - 1) + (offset / 2)
      val r     = sqrt(1 - pow(y, 2))
      val phi   = ((i + step) % num) * increment
      val x     = cos(phi) * r
      val z     = sin(phi) * r
      points(i) = Pt3(x, y, z)
    }

    points
  }

  final val RasterSize = 2970204

  def main(args: Array[String]): Unit = {
    val config = Config()
    if (!CoordFile(0).exists) {
      println("Creating coordinate files...")
      createCoordinateFiles()
      println("Done.")
    }
    println("Testing application and Voronoi mapping...")
    testApplyAndMapVoronoi(config)
  }

  def createCoordinateFiles(): Unit = {
    val all = fibonacciSphere(RasterSize)
    for (vi <- 0 until 5) {
      val tc          = voronoiCentersPt3(vi)
      val polyIndices = voronoiPolygons(vi)

      val fOut  = CoordFile(vi)
      val afOut = AudioFile.openWrite(fOut, AudioFileSpec(numChannels = 2, sampleRate = 96000.0))
      val afBuf = afOut.buffer(8192)
      var off   = 0

      def flush(): Unit =
        if (off > 0) {
          afOut.write(afBuf, 0, off)
          off = 0
        }

      def put(posH: Double, posV: Double): Unit = {
        afBuf(0)(off) = posH.toFloat
        afBuf(1)(off) = posV.toFloat
        off += 1
        if (off == 8192) flush()
      }

      try {
        val tca   = voronoiCentersPt3.toArray
        var ai    = 0
        while (ai < all.length) {
          val p = all(ai)
          var vj = 0
          var bestDist = Double.MaxValue
          var bestIdx = 0
          while (vj < 5) {
            val q = tca(vj)
            val dist = q.centralAngle(p)
            if (bestDist > dist) {
              bestDist  = dist
              bestIdx   = vj
            }
            vj += 1
          }
          if (bestIdx == vi) {
            var bestExt     = Double.PositiveInfinity
            var bestPosV    = 0.0
            var bestPosH    = 0.0
            var bestOk      = true
            var polyAccum   = 0.0
            import kollflitz.Ops._
            polyIndices.foreachPair { (pi1, pi2) =>
              val c1        = voronoiCornersPt3(pi1)
              val c2        = voronoiCornersPt3(pi2)
              val n         = c1 normalizedCross c2
              val nn        = p cross tc
              val v         = n cross c1
              val h0        = -(nn dot v) * c1 + (nn dot c1) * v
              val h         = h0.normalized
              val ext       = h.centralAngle(tc)
              val d3        = c1.centralAngle(c2) // outline polygon segment length
              if (ext < bestExt) {
                bestExt       = ext
                val d1        = c1.centralAngle(h)
                if (d1 > d3) {
                  // bestOk  = false
                } else {
                  val d2      = h.centralAngle(c2)
                  if (d2 > d3) {
                    // bestOk = false
                  } else {
//                    bestPosV  = h.centralAngle(p) / ext
                    bestPosV  = p.centralAngle(tc) / ext    // centre = 0, periphery = 1
                    bestPosH  = polyAccum + d1
                    bestOk    = true
                  }
                }
//                bestPosH      = polyAccum + d1
              }
              polyAccum += d3
            }
            if (!bestOk) {
              println(s"Failed to get position. vi = $vi, ai = $ai")
            }
            bestPosH /= polyAccum
            put(bestPosH, bestPosV)
          }
          ai += 1
        }

        flush()

      } finally {
        afOut.close()
      }
    }
  }

  def testApplyAndMapVoronoi(config: Config): Unit = {
    import config._
    val tempIn    = file("/data/projects/Koerper/audio_work/us-180512-approach-continuous-motu-%d.aif")
    val fCalib    = file("/data/temp/test-calib.aif")
    val specCalb  = AudioFile.readSpec(fCalib)
    //    require (specIn.sampleRate == sr)
    require (specCalb.numFrames == numBands)
    val tempOut   = file("/data/temp/test-removeMap-%d.aif")

    val g = Graph {
      import graph._
      for (ch <- 0 until 1) {
        val fIn       = formatTemplate(tempIn , ch + 1)
        val fOut      = formatTemplate(tempOut, ch + 1)
        val specIn    = AudioFile.readSpec(fIn)
        val numWin0   = calcNumWin(specIn.numFrames, config)
        val numWin    = math.min(numWin0, 8192)
        println(s"[${ch + 1}] numWin = $numWin; numWin0 = $numWin0, numBands = $numBands; winStep = ${calcWinStep(config)}")
        val in        = AudioFileIn(file = fIn    , numChannels = 1) * gainIn
        val calib     = AudioFileIn(file = fCalib , numChannels = 1)
        val calibR    = RepeatWindow(calib, size = numBands, num = numWin)
        val constQ    = analyze(in, config)
        val norm      = constQ.sqrt // / dbMax.dbAmp
        val min       = norm min calibR
        val thresh    = norm - min
        val rot       = thresh // RotateFlipMatrix(thresh, rows = numWin, columns = numBands, mode = RotateFlipMatrix.Rot90CCW)
//        RunningMin(rot).last.poll(0, "min")
//        RunningMax(rot).last.poll(0, "max")
        val max       = rot.ampDb.linLin(dbMin, dbMax, 0.0, 1.0).clip()
        val fCoord    = CoordFile(ch)
        val coord     = AudioFileIn(fCoord, numChannels = 2)
        val posH      = coord.out(0)
        val posV      = coord.out(1)
        posH.poll(label = "posH")
        posV.poll(label = "posV")
        val scanned   = ScanImage(max, width = numBands, height = numWin, x = posV, y = posH, zeroCrossings = 0)
        AudioFileOut(fOut, AudioFileSpec(numChannels = 1, sampleRate = 96000.0), in = scanned)
      }
    }

    val ctrl = Control()
    ctrl.run(g)
    import ctrl.config.executionContext
    ctrl.status.foreach { _ =>
      sys.exit()
    }
  }

  def testFibo(): Unit = {
    val arr = fibonacciSphere(RasterSize)
    val numDist = arr.distinct.length
    println(f"distinct are $numDist or ${numDist * 100.0 / RasterSize}%g%%")
    val tca = voronoiCentersPt3.toArray
    var vi = 0
    while (vi < 5) {
      var ai = 0
      var count = 0
      while (ai < arr.length) {
        val p = arr(ai)
        var vj = 0
        var bestDist = Double.MaxValue
        var bestIdx  = 0
        while (vj < 5) {
          val q = tca(vj)
          val dist = q.centralAngle(p)
          if (bestDist > dist) {
            bestDist  = dist
            bestIdx   = vj
          }
          vj += 1
        }
        if (bestIdx == vi) count += 1
        ai += 1
      }
      println(f"[$vi] count = $count, or ${count * 100.0 / RasterSize}%g%%")
      vi += 1
    }
  }

  def runPerf(): Unit = {
    val extent = 1080
    perfTest(extent); perfTest(extent)
    val (n, _) = perfTest(extent)
    println(s"Took $n ms for extent = $extent")
  }

  private final val TWO_POW_450   = java.lang.Double.longBitsToDouble(0x5C10000000000000L)
  private final val TWO_POW_N450  = java.lang.Double.longBitsToDouble(0x23D0000000000000L)
  private final val TWO_POW_750   = java.lang.Double.longBitsToDouble(0x6ED0000000000000L)
  private final val TWO_POW_N750  = java.lang.Double.longBitsToDouble(0x1110000000000000L)

  // cf. https://stackoverflow.com/questions/3764978/why-hypot-function-is-so-slow
  def fastHypot(x: Double, y: Double): Double = {
    var _x = Math.abs(x)
    var _y = Math.abs(y)
    if (_y < _x) {
      val a = _x
      _x = _y
      _y = a
    }
    else if (!(_y >= _x)) { // Testing if we have some NaN.
      if ((_x == java.lang.Double.POSITIVE_INFINITY) || (_y == java.lang.Double.POSITIVE_INFINITY)) return java.lang.Double.POSITIVE_INFINITY
      else return java.lang.Double.NaN
    }
    if (_y - _x == _y) { // x too small to substract from y
      _y
    }
    else {
      var factor = .0
      if (_x > TWO_POW_450) { // 2^450 < x < y
        _x *= TWO_POW_N750
        _y *= TWO_POW_N750
        factor = TWO_POW_750
      }
      else if (_y < TWO_POW_N450) { // x < y < 2^-450
        _x *= TWO_POW_750
        _y *= TWO_POW_750
        factor = TWO_POW_N750
      }
      else factor = 1.0
      factor * Math.sqrt(_x * _x + _y * _y)
    }
  }

  def perfTest(extent: Int): (Long, Double) = {
//    val gen     = new RandomKdFloat2dTreeGenerator
//    val kdTree  = gen.generate(2970204)
//    val nn      = new NNSolverFloat2d(kdTree)

    val t1 = System.currentTimeMillis()
    var foo = 0.0
    var xi = 0
    while (xi < extent) {
      var yi = 0
      while (yi < extent) {
        val x = xi.toDouble / extent * 2 - 1
        val y = yi.toDouble / extent * 2 - 1
        val d = x*x + y*y // fastHypot(x, y)
        if (d > 1.0) {
          foo += 1
        } else {
          val z = Math.sqrt(1 - d)
          val bar = 0.0 // nn.getAClosePoint(x.toFloat, y.toFloat)
          foo += z + bar
        }
        yi += 1
      }
      xi += 1
    }
    val t2 = System.currentTimeMillis()
    (t2 - t1, foo)
  }
}
