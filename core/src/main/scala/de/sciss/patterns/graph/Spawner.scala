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

object Spawner {
  trait Queue[Tx] {
    type Ref

    def par(pat: Pat[Event])(implicit tx: Tx): Ref
    def seq(pat: Pat[Event])(implicit tx: Tx): Unit

    def suspend(ref: Ref)(implicit tx: Tx): Unit

    def advance(seconds: Double)(implicit tx: Tx): Unit

    implicit val context: Context[Tx]
    implicit def random : Random [Tx]
  }
}
final case class Spawner(fun: Spawner.Queue[_] => Unit) extends Pattern[Event] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Event] = {
    val queue = new QueueImpl[Tx](tx)
    fun(queue)
    queue.iterator
  }
}
