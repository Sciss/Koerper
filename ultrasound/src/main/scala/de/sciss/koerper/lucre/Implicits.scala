package de.sciss.koerper.lucre

import de.sciss.lucre.swing.CellView

object Implicits {
  final class OptionCellViewOps[Tx, A](private val in: CellView[Tx, Option[A]]) extends AnyVal {
    def getOrElse(default: => A): CellView[Tx, A] = new impl.OptionCellViewGetOrElse[Tx, A](in, default)
  }
}
