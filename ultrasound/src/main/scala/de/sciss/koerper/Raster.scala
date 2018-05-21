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

import de.sciss.koerper.Geom._
import de.sciss.koerper.Voronoi.voronoiCentersPt3

import scala.math._

object Raster {
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

  def main(args: Array[String]): Unit = {
    val num = 2970204
    val arr = fibonacciSphere(num = num)
    val numDist = arr.distinct.length
    println(f"distinct are $numDist or ${numDist * 100.0 / num}%g%%")
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
      println(f"[$vi] count = $count, or ${count * 100.0 / num}%g%%")
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
