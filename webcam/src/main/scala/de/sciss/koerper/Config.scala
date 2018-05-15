/*
 *  Config.scala
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

final case class Config(camName: Option[String] = None, camWidth: Int = 640, camHeight: Int = 480,
                        listCams: Boolean = false)