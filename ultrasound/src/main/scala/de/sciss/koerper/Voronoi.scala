package de.sciss.koerper

import java.awt.Color
import java.awt.image.BufferedImage

import de.sciss.file._
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

  private final val PiH = math.Pi/2

  final case class Pt3(x: Double, y: Double, z: Double) {
    def length: Double = (x.squared + y.squared + z.squared).sqrt

    def * (d: Double): Pt3 = Pt3(x * d, y * d, z * d)

    def normalized: Pt3 = this * length.reciprocal

    def dot(that: Pt3): Double = this.x * that.x + this.y * that.y + this.z * that.z

    def distance(that: Pt3): Double = distanceSq(that).sqrt

    def distanceSq(that: Pt3): Double = {
      val dx = this.x - that.x
      val dy = this.y - that.y
      val dz = this.z - that.z
      dx.squared + dy.squared + dz.squared
    }

    def centralAngle(that: Pt3): Double = {
      val d = this distance that
      2 * asin(d/2)
    }

    def centralAngleBla(that: Pt3): Double = {
      val thetaA  = acos(this.z)
      val phiA    = atan2(this.y, this.x)
      val latA    = PiH - thetaA
      val lonA    = phiA

      val thetaB  = acos(that.z)
      val phiB    = atan2(that.y, that.x)
      val latB    = PiH - thetaB
      val lonB    = phiB

//      2 * asin(sqrt(sin((latA absDif latB)/2).squared + cos(latA) * cos(latB) * sin((lonA absDif lonB)/2).squared))

      acos(latA.sin * latB.sin + latA.cos * latB.cos * (lonA absDif lonB).cos)
    }

//    def centralAngle(that: Pt3): Double = {
//      val thetaA  = acos(this.z)
//      val phiA    = atan2(this.y, this.x)
//      val latA    = PiH - thetaA
//      val lonA    = phiA
//      val cosLatA = cos(latA)
//      val nax     = cosLatA * cos(lonA)
//      val nay     = cosLatA * sin(lonA)
//      val naz     = sin(latA)
//
//      val thetaB  = acos(that.z)
//      val phiB    = atan2(that.y, that.x)
//      val latB    = PiH - thetaB
//      val lonB    = phiB
//      val cosLatB = cos(latB)
//      val nbx     = cosLatB * cos(lonB)
//      val nby     = cosLatB * sin(lonB)
//      val nbz     = sin(latB)
//
//      acos(nax * nbx + nay * nby + naz * nbz)
//    }

    def toPolar: Polar = {
      val theta = acos(z)
      val phi   = atan2(y, x)
      Polar(theta, phi)
    }

    def toLatLon: LatLon = {
      val theta = acos(z)
      val phi   = atan2(y, x)
      val lat   = PiH - theta
      val lon   = phi
      LatLon(lat, lon)
    }

    def rotateX(a: Double): Pt3 = {
      val cosA  = a.cos
      val sinA  = a.sin
      val yR    = y*cosA - z*sinA
      val zR    = y*sinA + z*cosA
      copy(y = yR, z = zR)
    }

    def rotateY(a: Double): Pt3 = {
      val cosA  = a.cos
      val sinA  = a.sin
      val xR    = x*cosA - z*sinA
      val zR    = x*sinA + z*cosA
      copy(x = xR, z = zR)
    }

    def rotateZ(a: Double): Pt3 = {
      val cosA  = a.cos
      val sinA  = a.sin
      val xR    = x*cosA - y*sinA
      val yR    = x*sinA + y*cosA
      copy(x = xR, y = yR)
    }
  }

  /** Polar (two angles) representation of a 3D point.
    *
    * @param theta aka elevation
    * @param phi   aka azimuth
    */
  final case class Polar(theta: Double, phi: Double) {
    def toCartesian: Pt3 = {
      val sinTheta = sin(theta)
      val x = sinTheta * cos(phi)
      val y = sinTheta * sin(phi)
      val z = cos(theta)
      Pt3(x, y, z)
    }

    def toLatLon: LatLon = {
      val lat   = PiH - theta
      val lon   = phi
      LatLon(lat, lon)
    }
  }

  final case class LatLon(lat: Double, lon: Double) {
    override def toString = f"[lat: $lat%1.2f, lon: $lon%1.2f]"

    def toPolar: Polar = {
      val theta = PiH - lat
      val phi   = lon
      Polar(theta, phi)
    }

    def toCartesian: Pt3 = toPolar.toCartesian

    // cf. https://en.wikipedia.org/wiki/Great-circle_distance
    // cf. https://en.wikipedia.org/wiki/Spherical_coordinate_system
    def centralAngle(that: LatLon): Double =
      acos(this.lat.sin * that.lat.sin + this.lat.cos * that.lat.cos * (this.lon absDif that.lon).cos)

    //    def centralAngle(that: Polar): Double =
    //      acos(this.theta.sin * that.theta.sin + this.theta.cos * that.theta.cos * (this.phi absDif that.phi).cos)
  }

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

  def main(args: Array[String]): Unit =
    testRender()

  def testRender(): Unit = {
    val extent    = 512
    val lightRef  = Pt3(-0.7, -0.7, 1).normalized
    val img       = new BufferedImage(extent, extent, BufferedImage.TYPE_INT_ARGB)

    (0 until 120).zipWithIndex.foreach { case (rot, ri) =>
      for (xi <- 0 until extent) {
        for (yi <- 0 until extent) {
          val x = xi.linLin(0, extent, -1.0, 1.0)
          val y = yi.linLin(0, extent, -1.0, 1.0)
          val d = math.hypot(x, y)
          if (d > 1.0) {
            img.setRGB(xi, yi, 0xFF000000)
          } else {
            val z   = math.sqrt(1 - d)
            val v0  = Pt3(x, y, z)
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
            val tb  = voronoiCornersPt3.minBy(_.centralAngle(v))
            val col = {
              if (true || tb.centralAngle(v) > 0.05) {
                val hue = voronoiCentersPt3.indexOf(tc).linLin(0, voronoiCentersPt3.size, 0f, 1f)
                Color.getHSBColor(hue, 1f, 1f)
              } else {
                Color.gray
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
}
