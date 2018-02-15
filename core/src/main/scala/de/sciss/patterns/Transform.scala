package de.sciss.patterns

trait Transform {
  def apply[A](in: Pat[A]): Pat[A]
}
