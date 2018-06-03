/*
 *  Raster.scala
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
package proto

import java.awt.geom.Ellipse2D
import java.awt.image.BufferedImage
import java.awt.{Color, RenderingHints}
import java.util

import de.sciss.file._
import de.sciss.fscape.stream.Control
import de.sciss.fscape.{Graph, graph}
import de.sciss.koerper.Koerper
import de.sciss.koerper.proto.Calibration.formatTemplate
import de.sciss.koerper.proto.DopplerTest.{analyze, calcNumWin}
import de.sciss.koerper.proto.Geom._
import de.sciss.koerper.proto.Voronoi.{ScalarOps, voronoiCentersPt3, voronoiCornersPt3, voronoiPolygons}
import de.sciss.{kollflitz, numbers}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import javax.imageio.ImageIO

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.math._

object Raster {
//  private final val CoordCookie = 0x436F6F72  // "Coor"

  /*

  val nRing = (1 until ring).map { i =>
    val f = i.linLin(0, ring, 0.0, math.Pi).sin
    val n = (f * ring + 0.5).toInt
    n
  }

  nRing.sum // 2970204

   */

  def testRenderVoronoiImage2(): Unit = {
    val extent      = 1080
    val oval        = new Ellipse2D.Float
    val tempOut     = file("/data/temp/stoch-test/test-render-voronoi-%03d.png")

    val tableCoords = for (ch <- 0 until Koerper.numChannels) yield {
      readSphereCoordinateFile(ch)
    }
    val tablePixels = for (ch <- 0 until Koerper.numChannels) yield {
      val tempApp = file("/data/temp/test-removeMap-%d.aif")
      val fIn = formatTemplate(tempApp, ch + 1)
      val afIn = AudioFile.openRead(fIn)
      try {
        val buf = afIn.buffer(afIn.numFrames.toInt)
        afIn.read(buf)
        buf(0)
      } finally {
        afIn.close()
      }
    }

    val totalCoordNum = tableCoords.map(_.length/2).sum
    assert(totalCoordNum == RasterSize)

    //    val all = fibonacciSphere(RasterSize)

    (0 until 100).zipWithIndex.foreach { case (rot, ri) =>
      val fOut = formatTemplate(tempOut, ri + 1)
      if (!fOut.exists()) {
        val img       = new BufferedImage(extent, extent, BufferedImage.TYPE_INT_ARGB)
        val g         = img.createGraphics()
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )
        g.setColor(Color.black)
        g.fillRect(0, 0, extent, extent)

        val rnd = new util.Random(0L)
        for (ch <- 0 until Koerper.numChannels) {
          val tableCoord = tableCoords(ch)
          val tablePixel = tablePixels(ch)
          import numbers.Implicits._
          for (_ <- 0 until 40000) {
            val dot   = rnd.nextInt(tableCoord.length/2)
            val dotL  = dot << 1
            val theta = tableCoord(dotL)
            val phi   = tableCoord(dotL + 1)
            val v0    = Polar(theta, phi).toCartesian

            //            val v0 = all(rnd.nextInt(all.length))

            val v = {
              val v1 = v0.rotateX(rot * 6.0.toRadians)
              v1.rotateY(rot * 3.0.toRadians)
            }
            if (v.z < 0) {
              val bri = tablePixel(dot)
              g.setColor(Color.getHSBColor(ch.linLin(0, Koerper.numChannels, 0.0f, 1.0f), 1f - bri, bri))
              //              import numbers.Implicits._
              val x = v.x.linLin(-1, 1, 0, extent)
              val y = v.y.linLin(-1, 1, 0, extent)
              oval.setFrame(x - 1.0, y - 1.0, 2.0, 2.0)
              g.fill(oval)
              //        count += 1
            }
          }
        }
        g.dispose()
        ImageIO.write(img, "png", fOut)
        //    println(f"Hits $count of $N (or ${count * 100.0 / N}%g%%)")
      }
    }
  }

  def testRenderVoronoiImage(): Unit = {
    val extent      = 1080
    val oval        = new Ellipse2D.Float
    val tempOut     = file("/data/temp/stoch-test/test-render-voronoi-%03d.png")

    val tableCoords = for (ch <- 0 until Koerper.numChannels) yield {
      readSphereCoordinateFile(ch)
    }

    val totalCoordNum = tableCoords.map(_.length/2).sum
    assert(totalCoordNum == RasterSize)

//    val all = fibonacciSphere(RasterSize)

    (0 until 100).zipWithIndex.foreach { case (rot, ri) =>
      val fOut = formatTemplate(tempOut, ri + 1)
      if (!fOut.exists()) {
        val img       = new BufferedImage(extent, extent, BufferedImage.TYPE_INT_ARGB)
        val g         = img.createGraphics()
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )
        g.setColor(Color.black)
        g.fillRect(0, 0, extent, extent)

        val rnd = new util.Random(0L)
        for (ch <- 0 until Koerper.numChannels) {
          val tableCoord = tableCoords(ch)
          import numbers.Implicits._
          g.setColor(Color.getHSBColor(ch.linLin(0, Koerper.numChannels, 0.0f, 1.0f), 1f, 1f))
          for (_ <- 0 until 10000) {
            val dotL = rnd.nextInt(tableCoord.length) & ~1
            val theta = tableCoord(dotL)
            val phi   = tableCoord(dotL + 1)
            val v0    = Polar(theta, phi).toCartesian

//            val v0 = all(rnd.nextInt(all.length))

            val v = {
              val v1 = v0.rotateX(rot * 6.0.toRadians)
              v1.rotateY(rot * 3.0.toRadians)
            }
            if (v.z < 0) {
//              import numbers.Implicits._
              val x = v.x.linLin(-1, 1, 0, extent)
              val y = v.y.linLin(-1, 1, 0, extent)
              oval.setFrame(x - 1.0, y - 1.0, 2.0, 2.0)
              g.fill(oval)
              //        count += 1
            }
          }
        }
        g.dispose()
        ImageIO.write(img, "png", fOut)
        //    println(f"Hits $count of $N (or ${count * 100.0 / N}%g%%)")
      }
    }
  }

  def testRenderStochasticImage(): Unit = {
//    val tableCoord = mkSphereCoordinatesTable()
    val (tableCoord, tableData, tableSize) = mkStochasticTable()
    println(s"tableSize = $tableSize, RasterSize = $RasterSize")
    require (tableSize > 0)
    val N          = RasterSize / 15; // sqrt(RasterSize).toInt

    val extent    = 1080
    val oval      = new Ellipse2D.Float
    val tempOut   = file("/data/temp/stoch-test/test-render-stoch-%03d.png")

    (0 until 100 /* 1000 */).zipWithIndex.foreach { case (rot, ri) =>
      val fOut = formatTemplate(tempOut, ri + 1)
      if (!fOut.exists()) {
        val img       = new BufferedImage(extent, extent, BufferedImage.TYPE_INT_ARGB)
        val g         = img.createGraphics()
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )
        g.setColor(Color.black)
        g.fillRect(0, 0, extent, extent)
        g.setColor(Color.white)
        //    var count     = 0
        //    println(s"LAST ${tableData.last}")
        for (_ <- 0 until N) {
          val i0    = util.Arrays.binarySearch(tableData, 0, tableSize, random().toFloat)
          val i1    = if (i0 >= 0) i0 else -(i0 - 1)
          val dot   = if (i1 < tableData.length) i1 else tableData.length - 1
          val dotL  = dot << 1
          val theta = tableCoord(dotL)
          val phi   = tableCoord(dotL + 1)
          val v0    = Polar(theta, phi).toCartesian
          val v = {
            val v1 = v0.rotateX(rot * 6.0 /* 0.23 */.toRadians)
            v1.rotateY(rot * 3.0 /* 0.11 */.toRadians)
          }
          if (v.z < 0) {
            import numbers.Implicits._
            val x = v.x.linLin(-1, 1, 0, extent)
            val y = v.y.linLin(-1, 1, 0, extent)
            oval.setFrame(x - 1.0, y - 1.0, 2.0, 2.0)
            g.fill(oval)
            //        count += 1
          }
        }
        g.dispose()
        ImageIO.write(img, "png", fOut)
        //    println(f"Hits $count of $N (or ${count * 100.0 / N}%g%%)")
      }
    }
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
    val config = ConstQConfig()
    if (!Koerper.VoronoiCoordFile(0).exists) {
      println("Creating coordinate files...")
      createCoordinateFiles()
      println("Done.")
    }
    val tempApp = file("/data/temp/test-removeMap-%d.aif")
    if (!formatTemplate(tempApp, 1).exists()) {
      println("Testing application and Voronoi mapping...")
      testApplyAndMapVoronoi(config)
      println("Done.")
    }

    testRenderStochasticImage()
//    testRenderVoronoiImage()
//    testRenderVoronoiImage2()
  }

  def mkSphereCoordinatesTable(): Array[Float] = {
    val buf = new Array[Float](RasterSize << 1)
    var off = 0
    var ch = 0
    while (ch < Koerper.numChannels) {
      val n = readSphereCoordinateFile(buf, off = off, ch = ch)
      off += n
      ch += 1
    }
    buf
  }

  def mkStochasticTable(chStart: Int = 0, chStop: Int = Koerper.numChannels): (Array[Float], Array[Float], Int) = {
    val bufCoord= new Array[Float](RasterSize << 1)
    val bufPix  = new Array[Float](RasterSize)
    val tempIn  = Koerper.auxDir / "test-removeMap-%d.aif"
    val afBuf   = new Array[Array[Float]](1)
    afBuf(0)    = bufPix
    var off     = 0
    var sum     = 0.0
    var ch = chStart // 0
    while (ch < chStop /* Koerper.numChannels */) {
      val fIn   = formatTemplate(tempIn, ch + 1)
      val n1    = readSphereCoordinateFile(bufCoord, off = off, ch = ch)
      val afIn  = AudioFile.openRead(fIn)
      val n     = afIn.numFrames.toInt
      require (n == n1, s"n = $n, n1 = $n1")

      afIn.read(afBuf, off, n)
      var read = off
      val stop = read + n
      while (read < stop) {
        val value = bufPix(read)
        if (value > 0.0) {
          sum += value
          bufPix  (off) = sum.toFloat
          val offC  = off  << 1
          val readC = read << 1
          bufCoord(offC)      = bufCoord(readC)
          bufCoord(offC + 1)  = bufCoord(readC + 1)
          off += 1
        }
        read += 1
      }

      ch += 1
    }
//    require (off == RasterSize)
    val gain = (1.0 / sum).toFloat
    val stop = off
    off = 0
    while (off < stop) {
      bufPix(off) *= gain
      off += 1
    }
    (bufCoord, bufPix, stop)
  }

  def readSphereCoordinateFile(ch: Int): Array[Float] = {
    val fIn   = Koerper.SphereCoordFile(ch)
    val spec  = AudioFile.readSpec(fIn)
    val n     = spec.numFrames.toInt
    val buf   = new Array[Float](n << 1)
    readSphereCoordinateFile(buf, 0, ch)
    buf
  }

  /**
    *
    * @param buf  the buffer to read the interleaved (theta, phi) tuples into
    * @param off  "logical" offset, i.e. actual buffer offset divided by two
    * @param ch   the channel from 0 until numChannels
    * @return     "logical" offset, i.e. actual buffer offset divided by two
    */
  def readSphereCoordinateFile(buf: Array[Float], off: Int, ch: Int): Int = {
    val fIn = Koerper.SphereCoordFile(ch)
    val af  = AudioFile.openRead(fIn)
    try {
      val afBuf = af.buffer(8192)
      val bh    = afBuf(0)
      val bv    = afBuf(1)
      val n     = af.numFrames.toInt
      var off1  = off << 1
      var rem   = n
      while (rem > 0) {
        val chunk = math.min(8192, rem)
        af.read(afBuf, 0, chunk)
        var i = 0
        while (i < chunk) {
          val theta = bh(i)
          val phi   = bv(i)
          buf(off1) = theta ; off1 += 1
          buf(off1) = phi   ; off1 += 1
          i += 1
        }
        rem -= chunk
      }
      n

    } finally {
      af.close()
    }
  }

  def createCoordinateFiles(): Unit = {
    val all = fibonacciSphere(RasterSize)
    for (vi <- 0 until Koerper.numChannels) {
      val tc          = voronoiCentersPt3 (vi)
      val polyIndices = voronoiPolygons   (vi)

      val fVOut       = Koerper.VoronoiCoordFile  (vi)
      val fSpOut      = Koerper.SphereCoordFile   (vi)
      // sr is arbitrary
      val afVOut      = AudioFile.openWrite(fVOut , AudioFileSpec(numChannels = 2, sampleRate = 96000.0))
      val afSphOut    = AudioFile.openWrite(fSpOut, AudioFileSpec(numChannels = 2, sampleRate = 96000.0))
      val afVBuf      = afVOut  .buffer(8192)
      val afSphBuf    = afSphOut.buffer(8192)
      var off         = 0

      def flush(): Unit =
        if (off > 0) {
          afVOut  .write(afVBuf   , 0, off)
          afSphOut.write(afSphBuf , 0, off)
          off = 0
        }

      def put(posH: Double, posV: Double, theta: Double, phi: Double): Unit = {
        require (!(theta == 0.0 && phi == 0.0))

        afVBuf  (0)(off) = posH .toFloat
        afVBuf  (1)(off) = posV .toFloat
        afSphBuf(0)(off) = theta.toFloat
        afSphBuf(1)(off) = phi  .toFloat
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
          var bestIdx = -1
          while (vj < Koerper.numChannels) {
            val q = tca(vj)
            val dist = q.centralAngle(p)
            if (bestDist > dist) {
              bestDist  = dist
              bestIdx   = vj
            }
            vj += 1
          }
          require (bestIdx >= 0)
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
//            val r  = p
            val pp = p.toPolar
//            val q  = pp.toCartesian
            put(posH = bestPosH, posV = bestPosV, theta = pp.theta, phi = pp.phi)
          }
          ai += 1
        }

        flush()

      } finally {
        afVOut  .close()
        afSphOut.close()
      }
    }
  }

  def testApplyAndMapVoronoi(config: ConstQConfig): Unit = {
    import config._
    val tempIn    = file("/data/projects/Koerper/audio_work/us-180512-approach-continuous-motu-%d.aif")
    val fCalib    = file("/data/temp/test-calib.aif")
    val specCalb  = AudioFile.readSpec(fCalib)
    //    require (specIn.sampleRate == sr)
    require (specCalb.numFrames == numBands)
    val tempOut   = file("/data/temp/test-removeMap-%d.aif")

    val g = Graph {
      import graph._
      for (ch <- 0 until Koerper.numChannels) {
        val fIn       = formatTemplate(tempIn , ch + 1)
        val fOut      = formatTemplate(tempOut, ch + 1)
        val specIn    = AudioFile.readSpec(fIn)
        val numWin0   = calcNumWin(specIn.numFrames, config)
        val numWin    = math.min(numWin0, 8192)
//        println(s"[${ch + 1}] numWin = $numWin; numWin0 = $numWin0, numBands = $numBands; winStep = ${calcWinStep(config)}")
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
        val fCoord    = Koerper.VoronoiCoordFile(ch)
        val coord     = AudioFileIn(fCoord, numChannels = 2)
        val posH      = coord.out(0) * numWin
        val posV      = coord.out(1) * numBands
//        posH.poll(label = "posH")
//        posV.poll(label = "posV")
        val scanned   = ScanImage(max, width = numBands, height = numWin, x = posV, y = posH, zeroCrossings = 0)
        // sr is arbitrary
        AudioFileOut(fOut, AudioFileSpec(numChannels = 1, sampleRate = 44100.0), in = scanned)
      }
    }

    val ctrl = Control()
    ctrl.run(g)
//    import ctrl.config.executionContext
    Await.result(ctrl.status, Duration.Inf)
//    ctrl.status.foreach { _ =>
//      sys.exit()
//    }
  }

  def testFibo(): Unit = {
    val arr = fibonacciSphere(RasterSize)
    val numDist = arr.distinct.length
    println(f"distinct are $numDist or ${numDist * 100.0 / RasterSize}%g%%")
    val tca = voronoiCentersPt3.toArray
    var vi = 0
    while (vi < Koerper.numChannels) {
      var ai = 0
      var count = 0
      while (ai < arr.length) {
        val p = arr(ai)
        var vj = 0
        var bestDist = Double.MaxValue
        var bestIdx  = 0
        while (vj < Koerper.numChannels) {
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
