package de.sciss.patterns
package graph

import de.sciss.patterns.Types.Top

case class Indices[T <: Top](in: Pat[T]) extends Pat.Int {
  private[patterns] def expand(implicit ctx: Context): Stream[Int] = ???

  def iterator(implicit ctx: Context): Stream[Int] = ???
  def embed   (implicit ctx: Context): Stream[Int] = ???
}