/*
 *  Koerper.scala
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

object Koerper {
  val baseDir: File = {
    val tmp = file("/data") / "projects" / "Koerper"
    if (tmp.isDirectory) tmp else userHome / "Documents" / "projects" / "Koerper"
  }

  val auxDir: File = baseDir / "aux"
}
