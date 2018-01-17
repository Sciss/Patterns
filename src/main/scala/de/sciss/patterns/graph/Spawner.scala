/*
 *  Spawner.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

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
  type EOut = Event#Out

  def iterator(implicit ctx: Context): Stream[EOut] = {
    val queue = new QueueImpl
    fun(queue)
    queue.iterator
  }
}
