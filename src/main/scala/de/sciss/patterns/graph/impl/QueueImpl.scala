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

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.immutable.{SortedMap => ISortedMap}
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
  final case class Ref(id: Int) {
    var time: Double = 0.0
  }

  sealed trait Cmd
  final case class Par    (ref: Ref, pat: Pat.Event)  extends Cmd
  final case class Suspend(ref: Ref)                  extends Cmd
  final case class Seq    (pat: Pat.Event)            extends Cmd
  final case class Advance(seconds: Double)           extends Cmd

//  sealed trait Blocking
//  final class SeqB(val it: Iterator[Event#Out]) extends Blocking {
//    var done: Boolean = it.hasNext
//    var curr: Event#Out = if (done) null else it.next()
//  }
//  final case class AdvanceB(stop: Double) extends Blocking
//  final case object Flush extends Blocking
}
final class QueueImpl(implicit val context: Context)
  extends Spawner.Queue {

  import QueueImpl.{Ref => _, _}

  type Ref = QueueImpl.Ref

  private[this] var refCnt  = 0
  private[this] var cmdRev  = List.empty[Cmd]

  implicit val random: Random = context.mkRandom()

  def par(pat: Pat.Event): Ref = {
    val ref = new Ref(refCnt)
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

  type Out = Event#Out

  def iterator: Iterator[Out] = new AbstractIterator[Out] {
    private[this] var pq              = ISortedMap.empty[Ref, Either[Iterator[Out], Iterator[Cmd]]]
    private[this] val cmdIt           = cmdRev.reverseIterator
    private[this] var now             = 0.0
    private[this] var pqStop          = 0.0
    private[this] var elem: Out       = _
//    private[this] var cmd : Blocking  = _
    private[this] var done            = false

    @tailrec
    private def init(): Unit =
      if (cmdIt.hasNext) {
        cmdIt.next() match {
          case Par(ref, pat) =>
            ref.time = now
            ??? // pq += ref -> pat.expand
            init()

          case Suspend(ref) =>
            pq -= ref
            init()

          case Seq(pat) =>
            ???
//            cmd = new SeqB(pat.expand)

          case Advance(seconds) =>
            ???
//            cmd = AdvanceB(now + seconds)
        }
      } else {
        done = true
      }

    private def advance(): Unit = ???

//    private def advance(): Unit = cmd match {
//      case b: SeqB =>
//        elem = b.it.next()
//        val d = Event.delta(elem)
//        if (d > 0.0) now += d
////        if (d >= 0.0 && it.hasNext) pq = pq.tail + ((time + d) -> it)
//
//        ???
//      case AdvanceB(stop) =>
//        ???
//      case Flush =>
//        ???
//    }

    init()
    if (!done) advance()

    def hasNext: Boolean = !done

    def next(): Out = {
      if (done) throw new NoSuchElementException("next on empty iterator")
      val res = elem
      advance()
      res
    }
  }
}