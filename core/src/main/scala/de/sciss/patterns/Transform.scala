package de.sciss.patterns

import de.sciss.lucre.stm.Base

trait Transform {
  final def apply[S <: Base[S], A](in: Pat[A])(implicit ctx: Context[S], tx: S#Tx): Pat[A] =
    applyOne(in).transform(this)

  protected def applyOne[A](in: Pat[A]): Pat[A]
}
