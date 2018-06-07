package de.sciss.koerper.lucre
package impl

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.CellView
import de.sciss.lucre.swing.impl.CellViewImpl

final class OptionCellViewGetOrElse[Tx, A](peer: CellView[Tx, Option[A]], default: => A)
  extends CellViewImpl.Basic[Tx, A] {

  type Repr = Unit

  def repr(implicit tx: Tx): Unit = ()

  def apply()(implicit tx: Tx): A = peer().getOrElse(default)

  def react(fun: Tx => A => Unit)(implicit tx: Tx): Disposable[Tx] =
    peer.react { implicit tx => opt => fun(tx)(opt.getOrElse(default)) }
}
