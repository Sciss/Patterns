/*
 *  QueueImpl.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph
package impl

import scala.collection.mutable
import scala.util.Random

object QueueImpl {
  object Ref {
    implicit object ord extends Ordering[Ref] {
      def compare(x: Ref, y: Ref): Int =
        if (x.time < y.time) -1 else if (x.time > y.time) 1 else {
          if (x.id < y.id  ) -1 else if (x.id   > y.id  ) 1 else 0
        }
    }
  }
  final case class Ref(id: Int)(var time: Double)

  sealed trait Cmd
  final case class Par    (ref: Ref, pat: Pat.Event)  extends Cmd
  final case class Seq    (pat: Pat.Event)            extends Cmd
  final case class Suspend(ref: Ref)                  extends Cmd
  final case class Advance(seconds: Double)           extends Cmd
}
final class QueueImpl(implicit val context: Context)
  extends Spawner.Queue {

  import QueueImpl.{Ref => _, _}

  type Ref = QueueImpl.Ref

  private[this] var refCnt  = 0
  private[this] var cmdRev  = List.empty[Cmd]

  implicit val random: Random = context.mkRandom()

  def par(pat: Pat.Event): Ref = {
    val ref = new Ref(refCnt)(0.0)  // time will be updated later
    refCnt += 1
//    pq += ref -> pat.expand
    cmdRev ::= Par(ref, pat)
    ref
  }

  def seq(pat: Pat.Event): Unit = cmdRev ::= Seq(pat)

  def suspend(ref: Ref): Unit = {
//    pq.remove(ref)
    cmdRev ::= Suspend(ref)
  }

  def advance(seconds: Double): Unit = cmdRev ::= Advance(seconds)

  def iterator: Iterator[Event#Out] = {
    val pq  = mutable.SortedMap.empty[Ref, Iterator[_]]
    var now = 0.0

    ???
  }
}