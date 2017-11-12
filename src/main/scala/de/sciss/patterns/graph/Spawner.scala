package de.sciss.patterns
package graph

import de.sciss.patterns.Types.Top

import scala.util.Random

object Spawner {
  trait Queue[T <: Top] {
    type Ref

    def par(pat: Pat[T]): Ref
    def seq(pat: Pat[T]): Ref
    def suspend(ref: Ref): Unit

    def advance(seconds: Double): Unit

    implicit def context: Context
    implicit def random : Random
  }
}
final case class Spawner[T <: Top](fun: Spawner.Queue[T] => Unit) extends Pattern[T] {
  type Out = T#Out

  def iterator(implicit ctx: Context): Iterator[Out] = {
    ???
  }
}
