package de.sciss.patterns
package graph

import de.sciss.patterns.graph.impl.QueueImpl

import scala.util.Random

object Spawner {
  trait Queue {
    type Ref

    def par(pat: Pat.Event): Ref
    def seq(pat: Pat.Event): Unit

    def suspend(ref: Ref): Unit

    def advance(seconds: Double): Unit

    implicit def context: Context
    implicit def random : Random
  }
}
final case class Spawner(fun: Spawner.Queue => Unit) extends Pattern[Event] {
  type Out = Event#Out

  def iterator(implicit ctx: Context): Iterator[Out] = {
    val queue = new QueueImpl
    fun(queue)
    queue.iterator
  }
}
