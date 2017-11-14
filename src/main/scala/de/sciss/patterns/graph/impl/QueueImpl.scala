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

  private def mkRef(): Ref = {
    val ref = new Ref(refCnt)
    refCnt += 1
    ref
  }

  def par(pat: Pat.Event): Ref = {
    val ref = mkRef()
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
    private[this] var pq = ISortedMap.empty[Ref, Either[Iterator[Out], Iterator[Cmd]]]

    if (cmdRev.nonEmpty) pq += mkRef() -> Right(cmdRev.reverseIterator)

//    private[this] var cmdRef = Map.empty[Ref, Ref]

    private[this] var now             = 0.0
    private[this] var pqStop          = 0.0
    private[this] var elem: Out       = _
//    private[this] var cmd : Blocking  = _
    private[this] var done            = false

    /*

 - if a pattern iterator is found, set `elem` to the next value; determine delta, and if the iterator is not
   exhausted, reschedule the thing
 - if a command iterator is found, look at the next command:
   - `Advance`: adjust stop-time     , put tail back on queue, and iterate
   - `Suspend`: remove pat from queue, put tail back on queue, and iterate
   - `Par`    : add pat iterator on q, put tail back on queue, and iterate
   - `Seq`    :
     - if pat iterator is empty, put tail back on queue, and iterate
     - otherwise, set `elem` to the next value; determine delta, and if the iterator is not
       exhausted, reschedule the thing;
       advance stop-time, put tail back on queue

     */

    @tailrec
    private def advance(): Unit =
      if (pq.nonEmpty) {
        val head = pq.head
        val tail = pq.tail
        head match {
          case (ref, l @ Left(patIt)) =>
            elem = patIt.next()
            val d = math.max(0.0, Event.delta(elem))
            ref.time += d
            pq = if (patIt.hasNext) {
              tail + (ref -> l)
            } else {
              tail
            }

          case (ref, r @ Right(cmdIt)) =>
            val cmd = cmdIt.next()

            def putBack(): Unit =
              pq = if (cmdIt.hasNext) {
                tail + (ref -> r)
              } else {
                tail
              }

            cmd match {
              case Par(refP, pat) =>
                refP.time = now
                val patIt = pat.expand
                pq += refP -> Left(patIt)
                putBack()
                advance()

              case Suspend(refP) =>
                pq -= refP
                advance()

              case Seq(pat) =>
                // - if pat iterator is empty, put tail back on queue, and iterate
                // - otherwise, set `elem` to the next value; determine delta, and if the iterator is not
                //   exhausted, reschedule the thing;
                //   advance stop-time, put tail back on queue
                ??? // refP.time = now
                val patIt = pat.expand

                ???
              // cmd = new SeqB(pat.expand)

              case Advance(d) =>
                ref.time += d
                putBack()
                advance()
            }
        }

      } else {
        done = true
      }

    advance()

    def hasNext: Boolean = !done

    def next(): Out = {
      if (done) throw new NoSuchElementException("next on empty iterator")
      val res = elem
      advance()
      res
    }
  }
}