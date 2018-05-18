package de.sciss.koerper

import java.awt.Color
import java.awt.image.BufferedImage

import de.sciss.file._
import de.sciss.numbers.Implicits._
import javax.imageio.ImageIO

object Voronoi {
/*
  Final points:

  Row:       1             2             3
  Col
    1:    0.921576   -0.299542   -0.246925
    2:   -0.126660    0.822730    0.554142
    3:   -0.042913    0.553573   -0.831694
    4:   -0.794917   -0.523188   -0.307217
    5:    0.042913   -0.553573    0.831694

 */

  case class Point3D(x: Double, y: Double, z: Double) {
    def length: Double = (x.squared + y.squared + z.squared).sqrt

    def * (d: Double): Point3D = Point3D(x * d, y * d, z * d)

    def normalized: Point3D = this * length.reciprocal

    def dot(that: Point3D): Double = this.x * that.x + this.y * that.y + this.z * that.z
  }

  val voronoiCenters = Seq(
    Point3D( 0.921576, -0.299542, -0.246925),
    Point3D(-0.126660,  0.822730,  0.554142),
    Point3D(-0.042913,  0.553573, -0.831694),
    Point3D(-0.794917, -0.523188, -0.307217),
    Point3D( 0.042913, -0.553573,  0.831694)
  )

  def main(args: Array[String]): Unit =
    testRender(voronoiCenters)

  def testRender(pt: Seq[Point3D]): Unit = {
    val extent = 1024
    val img = new BufferedImage(extent, extent, BufferedImage.TYPE_INT_ARGB)
    for (xi <- 0 until extent) {
      for (yi <- 0 until extent) {
        val x = xi.linLin(0, extent, -1.0, 1.0)
        val y = yi.linLin(0, extent, -1.0, 1.0)
        val d = math.hypot(x, y)
        if (d > 1.0) {
          img.setRGB(xi, yi, 0xFF000000)
        } else {
          val z = math.sqrt(1 - d)
          val v = Point3D(x, y, z)
          val t = pt.maxBy(_.dot(v))
          val col = {
            val hue = pt.indexOf(t).linLin(0, pt.size, 0f, 1f)
            Color.getHSBColor(hue, 1f, 1f)
          } .getRGB
          val r = ((col >> 16) & 0xFF) / 255f
          val g = ((col >>  8) & 0xFF) / 255f
          val b = ((col >>  0) & 0xFF) / 255f
          val l = ((Point3D(x, y, z) dot Point3D(-0.7, -0.7, 1).normalized) + 1.0) / 2.0
          val rl = r * l
          val gl = g * l
          val bl = b * l
          val rc = (rl * 255).toInt.clip(0, 255) << 16
          val gc = (gl * 255).toInt.clip(0, 255) <<  8
          val bc = (bl * 255).toInt.clip(0, 255) <<  0
          img.setRGB(xi, yi, 0xFF000000 | rc | gc | bc)
        }
      }
    }

    ImageIO.write(img, "png", userHome / "test.png")
  }
}
