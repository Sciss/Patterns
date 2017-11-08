package de.sciss.patterns

import de.sciss.patterns.Types.Top

trait PatView[A] {
  type T <: Top

  def toPat(in: A): Pat[T]
}
