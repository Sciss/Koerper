/*
 *  SphereGNGImpl.scala
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

package de.sciss.koerper.lucre
package impl

import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream, File, FileInputStream, FileOutputStream, InputStream, OutputStream}

import de.sciss.neuralgas.sphere.SphereGNG.Config
import de.sciss.neuralgas.sphere.impl.{EdgeImpl, LocImpl, LocVarImpl, NodeImpl}
import de.sciss.neuralgas.sphere.{Edge, Node}

import scala.annotation.tailrec
import scala.collection.mutable

final class SphereGNGImpl {
  private[this] val loc         = new LocVarImpl

//  // persisted
//  private[this] var nodeIdCount = Ref(0)
//  private[this] var numNodes    = Ref(0)
//
//  private[this] var nodes       = TArray.ofDim[NodeImpl](8192) // : Array[NodeImpl] = _
//  private[this] val edgeMap     = TMap.empty[Int, List[EdgeImpl]]
//

  // persisted
  private[this] var nodeIdCount = 0
  private[this] var numNodes    = 0

  private[this] var nodes   : Array[NodeImpl] = _
  private[this] val edgeMap     = mutable.Map.empty[Int, List[EdgeImpl]]

  // currently not persisted
  private[this] val rnd         = new scala.util.Random(0L) // (config.seed)

//  private[this] var _maxNodes: Int = _ // config.maxNodes0
//
//  def maxNodes: Int = _maxNodes
//
//  def maxNodes_=(value: Int): Unit =
//    _maxNodes = value

  private final val SPHERE_COOKIE = 0x53474E47 // "SGNG"

  def init(createTwo: Boolean)(implicit config: Config): this.type = {
    import config._
//    _maxNodes = maxNodes0
    rnd.setSeed(seed)
    nodes = new Array(maxNodes0)
    if (createTwo) {
      val n1 = mkRandomNode()
      val n2 = mkRandomNode()
      nodes(0) = n1
      nodes(1) = n2
      numNodes = 2
      observer.gngNodeInserted(n1)
      observer.gngNodeInserted(n2)
      addEdgeAndFire(n1, n2)
    } else {
      numNodes = 0
    }
    //      checkConsistency()
    this
  }

  def saveState(f: File): Unit = {
    val fOut = new FileOutputStream(f)
    try {
      writeState(fOut)
    } finally {
      fOut.close()
    }
  }

  def loadState(f: File)(implicit config: Config): Unit = {
    val fIn = new FileInputStream(f)
    try {
      readState(fIn)
    } finally {
      fIn.close()
    }
  }

  def writeState(out: OutputStream): Unit = {
    val dOut = new DataOutputStream(new BufferedOutputStream(out))
    import dOut._
    writeInt(SPHERE_COOKIE)
    writeInt(nodeIdCount)
    writeInt(numNodes)
    var i = 0
    while (i < numNodes) {
      val n = nodes(i)
      writeInt    (n.id     )
      writeDouble (n.theta  )
      writeDouble (n.phi    )
      writeDouble (n.utility)
      writeDouble (n.error  )
      writeShort  (n.numNeighbors)
      var j = 0
      while (j < n.numNeighbors) {
        val nb = n.neighbor(j)
        writeInt(nb.id)
        j += 1
      }
      i += 1
    }

    writeInt(edgeMap.size)
    edgeMap.foreach { case (id, buf) =>
      writeInt(id)
      writeInt(buf.size)
      buf.foreach { e =>
        writeInt(e.from.id)
        writeInt(e.to  .id)
        writeInt(e.age    )
      }
    }

    flush()
  }

  def readState(in: InputStream)(implicit config: Config): Unit = {
    val dIn = new DataInputStream(new BufferedInputStream(in))
    import dIn._
    val cookie = readInt()
    require (cookie == SPHERE_COOKIE,
      s"Unexpected cookie, found ${cookie.toHexString} instead of ${SPHERE_COOKIE.toHexString}")
    nodeIdCount = readInt()
    numNodes    = readInt()

    val neighborData  = new Array[Array[Int]](numNodes)
    val nodeMap       = mutable.Map.empty[Int, NodeImpl]

    var i = 0
    while (i < numNodes) {
      val id = readInt()
      val n = new NodeImpl(id = id, maxNeighbors = config.maxNeighbors)
      n.theta   = readDouble()
      n.phi     = readDouble()
      n.utility = readDouble()
      n.error   = readDouble()
      nodes(i)  = n
      nodeMap += n.id -> n
      val numNeighbors = readShort()
      val neighbors = new Array[Int](numNeighbors)
      neighborData(i) = neighbors
      var j = 0
      while (j < numNeighbors) {
        val nbId = readInt()
        neighbors(j) = nbId
        j += 1
      }
      i += 1
    }

    // resolve neighbours
    i = 0
    while (i < numNodes) {
      val neighbors = neighborData(i)
      val n         = nodes(i)
      var j = 0
      while (j < neighbors.length) {
        val nbId  = neighbors(j)
        val nb    = nodeMap(nbId)
        n.addNeighbor(nb)
        j += 1
      }
      i += 1
    }

    val edgeMapSz = readInt()
    i = 0
    edgeMap.clear()
    while (i < edgeMapSz) {
      val key = readInt()
      val numEdges = readInt()
//      val buf = mutable.Buffer.empty[EdgeImpl]
      val buf = List.newBuilder[EdgeImpl]
      var j = 0
      while (j < numEdges) {
        val fromId  = readInt()
        val toId    = readInt()
        val age     = readInt()
        val from    = nodeMap(fromId)
        val to      = nodeMap(toId  )
        val e       = new EdgeImpl(from, to)
        e.age       = age
        buf += e
        j += 1
      }
      edgeMap += key -> buf.result()
      i += 1
    }
  }

  private def mkNode()(implicit config: Config): NodeImpl = {
    val id = nodeIdCount
    nodeIdCount += 1
    new NodeImpl(id = id, maxNeighbors = config.maxNeighbors)
  }

  private def mkRandomNode()(implicit config: Config): NodeImpl = {
    val res   = mkNode()
    config.pd.poll(loc)
    res.updateTri(loc.theta, loc.phi)
    res
  }

  private[this] var stepCount = 0

  final class ConsistencyResult(val errors: List[String], val numNodes: Int, val numEdges: Int, val stepCount: Int) {
    def pretty: String = {
      val errS = if (errors.isEmpty) "Ok."
      else errors.mkString("Inconsistencies:\n  ", "\n  ", "")
      s"$errS\nnumNodes = $numNodes, numEdges = $numEdges, stepCount = $stepCount"
    }
  }

  def checkConsistency(): ConsistencyResult = synchronized {
    var errors = List.newBuilder[String]
    if (numNodes < 0) errors += s"numNodes $numNodes is less than zero"
    for (ni <- 0 until numNodes) {
      val n = nodes(ni)
      if (n == null) {
        errors += s"Node at index $ni is null"
      } else  for (j <- 0 until n.numNeighbors) {
        val m = n.neighbor(j)
        if (m == null) {
          errors += s"Neighbor $j of node ${n.id} at index $ni is null"
        } else {
          if (!m.isNeighbor(n)) {
            errors += s"Neighbor ${m.id} at neighbor index $j of node ${n.id} at index $ni is not detected as neighbor"
          }
          var k = 0
          var ok = false
          while (k < numNodes && !ok) {
            if (nodes(k) == m) ok = true
            k += 1
          }
          if (!ok) {
            errors += s"Neighbor ${m.id} at neighbor index $j of node ${n.id} at index $ni is not in node list"
          }

          //        val ei = findEdge(ni, nj)
          //        require (ei >= 0 && ei < numEdges)
        }
      }
    }
    new ConsistencyResult(errors.result(), numNodes = numNodes, numEdges = 0, stepCount = stepCount)
  }

  def step()(implicit config: Config): Unit = synchronized {
    import config._
    stepCount += 1
    if (nodes.length < maxNodes0) {
      val _n = new Array[NodeImpl](maxNodes0)
      System.arraycopy(nodes, 0, _n, 0, numNodes)
      nodes = _n
    }

    var maxError        = Double.NegativeInfinity
    var maxErrorN       = null : NodeImpl
    var minUtility      = Double.PositiveInfinity
    var minUtilityIdx   = -1
    var minDist         = Double.PositiveInfinity
    var minDistN        = null : NodeImpl
    var nextMinDist     = Double.PositiveInfinity
    var nextMinDistN    = null : NodeImpl
    var toDelete        = -1

    pd.poll(loc)
    loc.updateTri()

    val decay = 1.0 - config.beta

    var i = 0
    val _nn = numNodes
    while (i < _nn) {
      val n = nodes(i)

      // Mark a node without neighbors for deletion
      if (n.numNeighbors == 0) toDelete = i

      val d = centralAngle(n, loc)
      n.distance  = d
      n.error    *= decay
      n.utility  *= decay

      if (d < minDist) {
        nextMinDist   = minDist
        nextMinDistN  = minDistN
        minDist       = d
        minDistN      = n
      } else if (d < nextMinDist) {
        nextMinDist   = d
        nextMinDistN  = n
      }

      if (n.error > maxError) {
        maxError      = n.error
        maxErrorN     = n
      }

      if (n.utility < minUtility) {
        minUtility    = n.utility
        //          minUtilityN   = n
        minUtilityIdx = i
      }

      i += 1
    }

    if (_nn > 0) {
      val winner = minDistN // nodes(minDistIdx)
      adaptNode(n = winner, n1 = winner, n2 = loc, d = winner.distance, f = epsilon)
      winner.error += minDist
      val dU = if (_nn > 1) nextMinDist - minDist else minDist
      winner.utility += dU
      observer.gngNodeUpdated(winner)

      val numNb = winner.numNeighbors
      i = 0
      while (i < numNb) {
        val nb = winner.neighbor(i)
        assert(nb != null)
        adaptNode(n = nb, n1 = nb, n2 = loc, d = nb.distance, f = epsilon2)
        observer.gngNodeUpdated(nb)
        i += 1
      }

      // Connect two winning nodes
      if (minDistN != nextMinDistN && nextMinDistN != null) addEdgeAndFire(minDistN, nextMinDistN)

      // Calculate the age of the connected edges and delete too old edges
      ageEdgesOfNodeAndFire(minDistN)
    }

    // Insert and delete nodes
    val coin = rnd.nextDouble()
    val dang = coin < lambda
    if (dang && _nn < maxNodes0) {
      if (maxErrorN != null) {
        insertNodeBetweenAndFire(maxErrorN, maxErrorNeighbor(maxErrorN))
      } else {
        val n         = mkRandomNode()
        val numOld    = numNodes
        nodes(numOld) = n
        numNodes      = numOld + 1
        observer.gngNodeInserted(n)
      }
    }

    val _nn1 = numNodes
    if (/* (_nn1 > 2) && */ (dang && _nn1 > maxNodes0) || maxError > minUtility * utility) {
      if (minUtilityIdx >= 0) deleteNodeAndFire(minUtilityIdx)
    }

    // step += 1
  }

  def nodeIterator: Iterator[Node] = nodes.iterator.take(numNodes)
  def edgeIterator: Iterator[Edge] = edgeMap.valuesIterator.flatMap { buf =>
    buf.iterator.collect {
      case e if e.from.id < e.to.id => e
    }
  }

  private def maxErrorNeighbor(n: NodeImpl): NodeImpl = {
    var resErr  = Double.NegativeInfinity
    var res     = n
    val nNb     = n.numNeighbors
    var i       = 0
    while (i < nNb) {
      val nb = n.neighbor(i)
      if (nb.error > resErr) {
        resErr  = nb.error
        res     = nb
      }
      i += 1
    }

    res
  }

  private def addEdgeAndFire(from: NodeImpl, to: NodeImpl)(implicit config: Config): Unit = {
    import config._
    if (from.isNeighbor(to)) {
      val fromId  = from.id
      val buf     = edgeMap(fromId)
      val e       = buf.find(e => e.from.id == fromId || e.to.id == fromId).get
      if (e.age != 0) {
        e.age = 0
        observer.gngEdgeUpdated(e)
      }

    } else {
      if (from.canAddNeighbor && to.canAddNeighbor) {
        from.addNeighbor(to  )
        to  .addNeighbor(from)

//        val bufFrom = edgeMap.getOrElseUpdate(from.id, mutable.Buffer.empty)
//        val bufTo   = edgeMap.getOrElseUpdate(to  .id, mutable.Buffer.empty)
//        bufFrom += e
//        bufTo   += e
        var bufFrom = edgeMap.getOrElse(from.id, Nil)
        var bufTo   = edgeMap.getOrElse(to  .id, Nil)
        val e       = new EdgeImpl(from, to)
        bufFrom ::= e
        bufTo   ::= e
        edgeMap.put(from.id, bufFrom)
        edgeMap.put(to  .id, bufTo  )

        observer.gngEdgeInserted(e)
      }
    }
  }

  private def insertNodeBetweenAndFire(n1: NodeImpl, n2: NodeImpl)(implicit config: Config): Unit = {
    import config._
    val n   = mkNode()

    val alphaDecay = 1.0 - config.alpha
    n1.error *= alphaDecay
    n2.error *= alphaDecay

    // interpolate data
    n.error   = (n1.error   + n2.error  ) / 2.0
    n.utility = (n1.utility + n2.utility) / 2.0
    val d     = centralAngle(n1, n2)
    adaptNode(n = n, n1 = n1, n2 = n2, d = d, f = 0.5)

    deleteEdgeBetweenAndFire(n1, n2)

    val numOld = numNodes
    nodes(numOld) = n
    numNodes = numOld + 1
    observer.gngNodeInserted(n)
    addEdgeAndFire(n1, n)
    addEdgeAndFire(n2, n)
  }

  private def ageEdgesOfNodeAndFire(n: Node)(implicit config: Config): Unit = {
    import config._
    val edges = edgeMap.getOrElse(n.id, Nil)
    edges.foreach { e =>
      if (e == null) {
        println(s"WTF $stepCount")
      }
      e.age += 1
      if (e.age <= config.maxEdgeAge) {
        observer.gngEdgeUpdated(e)
      } else{
        deleteEdgeAndFire(e)
      }
    }
  }

  private def deleteNodeAndFire(ni: Int)(implicit config: Config): Unit = {
    import config._
    val n     = nodes(ni)
    val nNb   = n.numNeighbors

    var i = 0
    while (i < nNb) {
      deleteEdgeBetweenAndFire(n, n.neighbor(0))
      i += 1
    }

    val numNew    = numNodes - 1
    numNodes      = numNew
    nodes(ni)     = nodes(numNew)
    nodes(numNew) = null

    val m = edgeMap.remove(n.id)
    assert (m.forall(_.isEmpty))

    observer.gngNodeRemoved(n)
  }

//  @inline
//  private def remove[A](xs: mutable.Buffer[A], elem: A): Unit =
//    xs.remove(xs.indexOf(elem))

//  @inline
//  private def remove[A](xs: List[A], elem: A): List[A] = {
//    @tailrec def loop(rem: List[A], res: List[A]): List[A] = rem match {
//      case `elem` :: tail => res.reverse ::: tail
//      case hd     :: tail => loop(tail, hd :: res)
//      case Nil            => res.reverse
//    }
//
//    loop(xs, Nil)
//  }

  private def removeEdge(id: Int, e: EdgeImpl): Boolean = {
    @tailrec def loop(rem: List[EdgeImpl], res: List[EdgeImpl]): Boolean = rem match {
      case `e` :: tail => edgeMap(id) = res.reverse ::: tail; true
      case hd  :: tail => loop(tail, hd :: res)
      case Nil         => false
    }

    loop(edgeMap(id), Nil)
  }

  // takes care of removing neighbours as well
  private def deleteEdgeAndFire(e: EdgeImpl)(implicit config: Config): Unit = {
    import config._
    import e._
    from.removeNeighbor(to)
    to  .removeNeighbor(from)
    val fromId  = from.id
    val toId    = to  .id
//    remove(edgeMap(fromId), e)
//    remove(edgeMap(toId  ), e)
    assert (removeEdge(fromId, e))
    assert (removeEdge(toId  , e))

    observer.gngEdgeRemoved(e)
  }

  // takes care of removing neighbours as well.
  // allowed to call this when there _is_ no edge.
  private def deleteEdgeBetweenAndFire(from: Node, to: Node)(implicit config: Config): Unit = {
    val fromId  = from.id
    val buf     = edgeMap.getOrElse(from.id, Nil)
    val eOpt    = buf.find(e => e.from.id == fromId || e.to.id == fromId)
    eOpt.foreach(deleteEdgeAndFire)
  }

  private[this] final val PiH = math.Pi * 0.5

  //    // cf. https://math.stackexchange.com/questions/2799079/interpolating-two-spherical-coordinates-theta-phi/
  //    // N.B. this is actually slightly slower than the 'avform' version below based
  //    // on lat/lon
  //    private def adaptNode_VERSION(n: NodeImpl, n1: Loc, n2: Loc, d: Double, f: Double): Unit = {
  //      import Math._
  //      val x1      = n1.sinTheta * cos(n1.phi)
  //      val y1      = n1.sinTheta * sin(n1.phi)
  //      val z1      = n1.cosTheta
  //
  //      val x2      = n2.sinTheta * cos(n2.phi)
  //      val y2      = n2.sinTheta * sin(n2.phi)
  //      val z2      = n2.cosTheta
  //
  //      val kx0     = y1 * z2 - z1 * y2
  //      val ky0     = z1 * x2 - x1 * z2
  //      val kz0     = x1 * y2 - y1 * x2
  //      val k0l     = sqrt(kx0*kx0 + ky0*ky0 + kz0*kz0)
  //
  //      val kx      = kx0 / k0l
  //      val ky      = ky0 / k0l
  //      val kz      = kz0 / k0l
  //
  //      val ang     = acos(x1 * x2 + y1 * y2 + z1 * z2) // == d
  //
  //      val psi     = ang * f
  //      val cosPsi  = cos(psi)
  //      val sinPsi  = sin(psi)
  //      val cosPsiI = 1.0 - cosPsi
  //
  //      val k1d     = kx * x1 + ky * y1 + kz * z1
  //      val k1cx    = ky * z1 - kz * y1
  //      val k1cy    = kz * x1 - kx * z1
  //      val k1cz    = kx * y1 - ky * x1
  //
  //      val psiX    = x1 * cosPsi + k1cx * sinPsi + kx * k1d * cosPsiI
  //      val psiY    = y1 * cosPsi + k1cy * sinPsi + ky * k1d * cosPsiI
  //      val psiZ    = z1 * cosPsi + k1cz * sinPsi + kz * k1d * cosPsiI
  //
  //      val theta   = acos(psiZ)
  //      val phi     = atan2(psiY, psiX)
  //      n.updateTri(theta, phi)
  //    }

  // cf. http://edwilliams.org/avform.htm
  private def adaptNode(n: NodeImpl, n1: LocImpl, n2: LocImpl, d: Double, f: Double): Unit = {
    if (d == 0) {
      n.updateTri(n1.theta, n1.phi)
      return
    }

    import Math._
    //      val lat1      = PiH - n1.theta
    val lon1      = n1.phi
    //      val lat2      = PiH - n2.theta
    val lon2      = n2.phi

    val sinD      = sin(d)
    val a         = sin((1 - f) * d) / sinD
    val b         = if (f == 0.5) a else sin(f * d) / sinD

    //      val cosLat1 = cos(lat1)
    //      assert(abs(cosLat1 - n1.sinTheta) < 1.0e-4, s"$cosLat1 versus ${n1.sinTheta}")
    val cosLat1   = n1.sinTheta
    val cosLon1   = cos(lon1)
    //      val cosLat2 = cos(lat2)
    //      assert(abs(cosLat2 - n2.sinTheta) < 1.0e-4, s"$cosLat2 versus ${n2.sinTheta}")
    val cosLat2   = n2.sinTheta
    val cosLon2   = cos(lon2)
    //      val sinLat1 = sin(lat1)
    //      assert(abs(sinLat1 - n1.cosTheta) < 1.0e-4, s"$sinLat1 versus ${n1.cosTheta}")
    val sinLat1   = n1.cosTheta
    val sinLon1   = sin(lon1)
    //      val sinLat2 = sin(lat2)
    //      assert(abs(sinLat2 - n2.cosTheta) < 1.0e-4, s"$sinLat2 versus ${n2.cosTheta}")
    val sinLat2   = n2.cosTheta
    val sinLon2   = sin(lon2)
    val aCosLat1  = a * cosLat1
    val bCosLat2  = b * cosLat2
    val x         = aCosLat1 * cosLon1 + bCosLat2 * cosLon2
    val y         = aCosLat1 * sinLon1 + bCosLat2 * sinLon2
    val z         = a * sinLat1        + b * sinLat2
    val lat       = atan2(z, sqrt(x * x + y * y))
    val lon       = atan2(y, x)

    val theta     = PiH - lat
    val phi       = lon
    n.updateTri(theta, phi)
  }

  @inline
  private def centralAngle(n1: LocImpl, n2: LocImpl): Double = {
    import Math._
    acos(n1.cosTheta * n2.cosTheta + n1.sinTheta * n2.sinTheta * cos(n1.phi - n2.phi))
  }
}