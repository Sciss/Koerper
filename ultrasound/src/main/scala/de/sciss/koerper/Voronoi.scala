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
    override def toString = f"$productPrefix($x%g, $y%g, $z%g)"

    def length: Double = (x.squared + y.squared + z.squared).sqrt

    def * (d: Double): Pt3 = Pt3(x * d, y * d, z * d)

    def + (that: Pt3): Pt3 = Pt3(this.x + that.x, this.y + that.y, this.z + that.z)

    def normalized: Pt3 = this * length.reciprocal

    def dot(that: Pt3): Double = this.x * that.x + this.y * that.y + this.z * that.z

    def distance(that: Pt3): Double = distanceSq(that).sqrt

    def distanceSq(that: Pt3): Double = {
      val dx = this.x - that.x
      val dy = this.y - that.y
      val dz = this.z - that.z
      dx.squared + dy.squared + dz.squared
    }

    def cross(that: Pt3): Pt3 = {
      val xOut = this.y * that.z - this.z * that.y
      val yOut = this.z * that.x - this.x * that.z
      val zOut = this.x * that.y - this.y * that.x
      Pt3(xOut, yOut, zOut)
    }

    def normalizedCross (that: Pt3): Pt3 = (this cross that).normalized

    private def r8_asin(s: Double): Double = asin(s.clip2(1.0))

    // http://people.sc.fsu.edu/~jburkardt/m_src/geometry/r8_atan.m
    // John Burkardt, LGPL
    private def r8_atan(y: Double, x: Double): Double =
      if (x == 0.0) {
        if (y > 0.0)
          Pi / 2.0
        else if (y < 0.0)
          3.0 * Pi / 2.0
        else /* if (y == 0.0) */
          0.0

      } else if (y == 0.0) {
        if (x > 0.0)
          0.0
        else
          Pi

      } else {
        // We assume that ATAN2 is correct when both arguments are positive.

        val abs_y = abs(y)
        val abs_x = abs(x)

        val theta_0 = atan2(abs_y, abs_x)

        if (x > 0.0 && y > 0.0)
          theta_0
        else if (x < 0.0 && y > 0.0)
          Pi - theta_0
        else if (x < 0.0 && y < 0.0)
          Pi + theta_0
        else /* if (x > 0.0 && y < 0.0) */
          2.0 * Pi - theta_0
      }

    def centralAngleBlala(that: Pt3): Double = {
      val lat1 = r8_asin(this.z)
      val lon1 = r8_atan(this.y, this.x)

      val lat2 = r8_asin(that.z)
      val lon2 = r8_atan(that.y, that.x)

      val topSq = (cos(lat2) * sin(lon1 - lon2)).squared +
                  (cos(lat1) * sin(lat2) -
                   sin(lat1) * cos(lat2) * cos(lon1 - lon2)).squared

      val top = sqrt(topSq)

      val bot = sin(lat1) * sin(lat2) +
                cos(lat1) * cos(lat2) * cos(lon1 - lon2)

      val dist = atan2 ( top, bot )
      dist
    }

    def centralAngle(that: Pt3): Double = {
      val lat1 = r8_asin(this.z)
      val lon1 = r8_atan(this.y, this.x)

      val lat2 = r8_asin(that.z)
      val lon2 = r8_atan(that.y, that.x)

      val sSq = sin((lat1 - lat2) / 2.0).squared +
        cos(lat1) * cos(lat2) * sin((lon1 - lon2) / 2.0).squared

      val s = sqrt(sSq)

      val dist = 2.0 * asin(s)
      dist
    }

    def centralAngleNo(that: Pt3): Double = {
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
    testRender()

  def testRender(): Unit = {
    val extent    = 512
    val lightRef  = Pt3(-0.7, -0.7, 1).normalized
    val img       = new BufferedImage(extent, extent, BufferedImage.TYPE_INT_ARGB)

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
    val pos       = 1.0 - (H.centralAngle(testPt) / H.centralAngle(c0))
    println(s"B = $B, D = $D, c0 = $c0, testPt = $testPt, H = $H")
    println(f"pos = $pos%g")

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
}
