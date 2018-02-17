package de.sciss.patterns

trait Transform {
  final def apply[A](in: Pat[A]): Pat[A] = applyOne(in).transform(this)

  protected def applyOne[A](in: Pat[A]): Pat[A]
}
