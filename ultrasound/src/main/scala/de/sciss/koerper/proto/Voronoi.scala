/*
 *  Voronoi.scala
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
package proto

import java.awt.Color
import java.awt.image.BufferedImage

import de.sciss.file._
import de.sciss.intensitypalette.IntensityPalette
import de.sciss.koerper.proto.Geom._
import de.sciss.kollflitz
import de.sciss.kollflitz.Vec
import de.sciss.numbers.Implicits._
import javax.imageio.ImageIO

import scala.math._

object Voronoi {
/*
  Final points:

  Row:       1             2             3
  Col
    1:   -0.4434043079017067    0.8403773315015197   -0.3117026795402846
    2:   -0.3472017428955947   -0.4791608023789854    0.8061363874641737
    3:    0.6098008647130453    0.5377071602341313    0.5822490147936434
    4:   -0.6098008647130450   -0.5377071602341316   -0.5822490147936435
    5:    0.7906060507973007   -0.3612165291225340   -0.4944337079238889

 */

  val voronoiCentersPt3: Vec[Pt3] = Vector(
    Pt3(-0.4434043079017067,  0.8403773315015197, -0.3117026795402846),
    Pt3(-0.3472017428955947, -0.4791608023789854,  0.8061363874641737),
    Pt3( 0.6098008647130453,  0.5377071602341313,  0.5822490147936434),
    Pt3(-0.6098008647130450, -0.5377071602341316, -0.5822490147936435),
    Pt3( 0.7906060507973007, -0.3612165291225340, -0.4944337079238889)
  )

  val voronoiCornersPt3: Vec[Pt3] = Vector(
    Pt3(-0.4344283119548618,  0.5635518379401856,  0.7026246278962103),
    Pt3( 0.5832579168556855,  0.6690444029634800, -0.4606406292202271),
    Pt3(-0.9798507864494710,  0.0826119330314294,  0.1818452771318628),
    Pt3( 0.0378354423610770,  0.1881044980547242, -0.9814199799845741),
    Pt3( 0.1238816323464809, -0.9921262884492874,  0.0184056767061907),
    Pt3( 0.6693041068410891, -0.5111863835405318,  0.5391850274705374)
  )

  val voronoiPolygons: Vec[Vec[Int]] = Vector(
    Vector(0, 1, 3, 2),
    Vector(0, 2, 4, 5),
    Vector(0, 5, 1),
    Vector(2, 3, 4),
    Vector(1, 5, 4, 3)
  )

  val voronoiOrders: Vec[Int] = voronoiPolygons.map(_.size)

  /*

    Voronoi orders:

     1:      4
     2:      4
     3:      3
     4:      3
     5:      4

  Voronoi polygons:

    1:         1         2         4         3
    2:         1         3         5         6
    3:         1         6         2
    4:         3         4         5
    5:         2         6         5         4

  Voronoi areas:

     1:     2.636232
     2:     2.636232
     3:     2.328837
     4:     2.328837
     5:     2.636232

   */

  implicit class ScalarOps(private val v: Double) extends AnyVal {
    def * (pt: Pt3): Pt3 = pt * v
  }

  def main(args: Array[String]): Unit =
    testRenderImage()

  def testRender(): Unit = {
    val extent    = 512
    val lightRef  = Pt3(-0.7, -0.7, 1).normalized
    val img       = new BufferedImage(extent, extent, BufferedImage.TYPE_INT_ARGB)

    // cf. https://math.stackexchange.com/questions/1720450/arc-intersection-on-a-sphere
    val c0        = voronoiCentersPt3(1)
    val testPt    = c0.rotateX(-10.toRadians).rotateY(13.toRadians)
    val B         = voronoiCornersPt3(voronoiPolygons(1)(0))
    val D         = voronoiCornersPt3(voronoiPolygons(1)(1))
    val n         = B normalizedCross D
    //    val N         = c0 cross testPt
    val N         = testPt cross c0
    val v         = n cross B
    val H0        = -(N dot v) * B + (N dot B) * v
    val H         = H0.normalized
    val pos1      = testPt.centralAngle(c0) / H.centralAngle(c0)
    val d1        = B.centralAngle(H)
    val d3        = B.centralAngle(D)
    val pos2      = d1 / d3
    println(s"B = $B, D = $D, c0 = $c0, testPt = $testPt, H = $H")
    println(f"pos1 = $pos1%g, pos2 = $pos2%g")

    (0 to 0).zipWithIndex.foreach { case (rot, ri) =>
      for (xi <- 0 until extent) {
        for (yi <- 0 until extent) {
          val x = xi.linLin(0, extent, -1.0, 1.0)
          val y = yi.linLin(0, extent, -1.0, 1.0)
          val d = hypot(x, y)
          if (d > 1.0) {
            img.setRGB(xi, yi, 0xFF000000)
          } else {
            val z   = sqrt(1 - d)
            val v0  = Pt3(x, y, z).normalized // ! this was bloody wrong
            val v   = {
              val v1 = v0.rotateX(rot * 6.toRadians)
              v1.rotateY(rot * 3.0.toRadians)
              //              val p0 = v0.toLatLon
              ////              val p1 = p0.copy(lon = p0.lon + rot.toRadians)
              //              val p1 = p0.copy(lat = p0.lat + rot.toRadians)
              //              p1.toCartesian
            }
            //            val tc  = voronoiCentersPt3.maxBy(_.dot(v))
            val tc  = voronoiCentersPt3.minBy(_.centralAngle(v))
            //            val tb  = voronoiCornersPt3.minBy(_.centralAngle(v))
            val da = 0.04
            val col = {
              if (v.centralAngle(H) < da) {
                Color.white
              } else
              if (v.centralAngle(c0) < da || v.centralAngle(testPt) < da ||
                v.centralAngle(B) < da || v.centralAngle(D) < da) {
                Color.darkGray
              } else {
                val hue = voronoiCentersPt3.indexOf(tc).linLin(0, voronoiCentersPt3.size, 0f, 1f)
                Color.getHSBColor(hue, 1f, 1f)
              }
            } .getRGB
            val r   = ((col >> 16) & 0xFF) / 255f
            val g   = ((col >>  8) & 0xFF) / 255f
            val b   = ((col >>  0) & 0xFF) / 255f
            val l   = (v0.dot(lightRef) + 1.0) / 2.0
            val rl  = r * l
            val gl  = g * l
            val bl  = b * l
            val rc  = (rl * 255).toInt.clip(0, 255) << 16
            val gc  = (gl * 255).toInt.clip(0, 255) <<  8
            val bc  = (bl * 255).toInt.clip(0, 255) <<  0
            img.setRGB(xi, yi, 0xFF000000 | rc | gc | bc)
          }
        }
      }

      ImageIO.write(img, "png", file(f"/data/temp/foo/test-rot-${ri+1}%03d.png"))
    }
  }

  def testRenderImage(): Unit = {
    val extent    = 1080
    val imgIn     = Array.tabulate(5)(ch => ImageIO.read(file(s"/data/projects/Koerper/material/us-test-remove-${ch+1}.png")))
    val imgOut    = new BufferedImage(extent, extent, BufferedImage.TYPE_INT_ARGB)
    val dirOut    = file("/data/temp/multi-faceApx/")
    dirOut.mkdirs()
    require(dirOut.isDirectory)

    (0 until 1000).zipWithIndex.foreach { case (rot, ri) =>
      val fOut = dirOut/f"testH-rot-${ri+1}%03d.png"
      if (fOut.length() == 0L) {
        for (xi <- 0 until extent) {
          for (yi <- 0 until extent) {
            val x = xi.linLin(0, extent, -1.0, 1.0)
            val y = yi.linLin(0, extent, -1.0, 1.0)
            val d = x*x + y*y // Raster.fastHypot(x, y) // hypot(x, y)
            if (d > 1.0) {
              imgOut.setRGB(xi, yi, 0xFF000000)
            } else {
              val z   = sqrt(1 - d)
              val v0  = Pt3(x, y, z) // .normalized
              val p  = {
                val v1 = v0.rotateX(rot * 6.toRadians)
                v1.rotateY(rot * 3.0.toRadians)
              }
              val tc    = voronoiCentersPt3.minBy(_.centralAngle(p))
              val vIdx  = voronoiCentersPt3.indexOf(tc)
              val col = {
                var bestExt     = Double.PositiveInfinity
                var bestPosV    = 0.0
                var bestPosH    = 0.0
                var polyAccum   = 0.0
                val polyIndices = voronoiPolygons(vIdx)
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
                  val d3        = c1.centralAngle(c2)
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
//                        bestPosV      = h.centralAngle(p) / ext
                        bestPosV      = p.centralAngle(tc) / ext
                        bestPosH      = polyAccum + d1
                      }
                    }
                  }
                  polyAccum += d3
                }
                bestPosH /= polyAccum
                // if (bestPos1 > 1.001) println(s"Ooops $bestPos1")
                val i     = imgIn(vIdx)
                val fy    = (1.0 - bestPosV) * i.getHeight
                val fx    = bestPosH * i.getWidth
                val iy    = fy.toInt
                val ix    = fx.toInt
                val wy2   = fy % 1.0
                val wy1   = 1.0 - wy2
                val wx2   = fx % 1.0
                val wx1   = 1.0 - wx2
                val iy1   = iy      .clip(0, i.getHeight - 1)
                val ix1   = ix      .clip(0, i.getWidth  - 1)
                val iy2   = (iy + 1).clip(0, i.getHeight - 1)
                val ix2   = (ix + 1).clip(0, i.getWidth  - 1)
                val rgb1  = (i.getRGB(ix1, iy1) & 0xFF) / 255.0
                val rgb2  = (i.getRGB(ix2, iy1) & 0xFF) / 255.0
                val rgb3  = (i.getRGB(ix1, iy2) & 0xFF) / 255.0
                val rgb4  = (i.getRGB(ix2, iy2) & 0xFF) / 255.0
                val l     = rgb1 * (wx1 * wy1) + rgb2 * (wx2 * wy1) + rgb3 * (wx1 * wy2) + rgb4 * (wx2 * wy2)
                IntensityPalette.apply(l.toFloat)

              }
              imgOut.setRGB(xi, yi, 0xFF000000 | col)
            }
          }
        }

        ImageIO.write(imgOut, "png", fOut)
      }
    }
  }
}
