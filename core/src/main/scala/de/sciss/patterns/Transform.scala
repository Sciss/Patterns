package de.sciss.patterns

trait Transform {
  final def apply[Tx, A](in: Pat[A])(implicit ctx: Context[Tx], tx: Tx): Pat[A] =
    applyOne(in).transform(this)

  protected def applyOne[A](in: Pat[A]): Pat[A]
}
