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
import scala.collection.immutable.{SortedMap => ISortedMap}
import scala.util.Random

object QueueImpl {

  sealed trait Cmd
  final case class Par    (ref: TimeRef, pat: Pat.Event)  extends Cmd
  final case class Suspend(ref: TimeRef)                  extends Cmd
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

  import QueueImpl._

  type Ref = TimeRef

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

  def iterator: Stream[Out] = new Stream[Out] {
    private[this] var pq = ISortedMap.empty[Ref, Stream[Either[Out, Cmd]]]

    if (cmdRev.nonEmpty) pq += mkRef() -> Stream.reverseIterator(cmdRev).map(Right(_))

//    private[this] var cmdRef = Map.empty[Ref, Ref]

//    private[this] var now       = 0.0
    private[this] var elem: Out = _
    private[this] var done      = false

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

    NOTE: the above is incorrect/unsolved for `Seq`. The implementation below
    fixes that by concatenating the tail of the command-containing iterator to
    the seq-pat iterator.

     */

    @tailrec
    private def advance(): Unit =
      if (pq.nonEmpty) {
        val (ref, it) = pq.head
        pq            = pq.tail

        def putBack(): Unit =
          if (it.hasNext) {
            pq += (ref -> it)
          }

        it.next() match {
          case Left(_elem) =>
            elem      = _elem
            val d     = math.max(0.0, Event.delta(_elem))
            val now   = ref.time
            ref.time += d
            putBack()
            if (pq.nonEmpty) {
              val nextTime = pq.firstKey.time
              elem += Event.keyDelta -> (nextTime - now)
            }

          case Right(cmd) =>
            cmd match {
              case Par(refP, pat) =>
                refP.time = ref.time // now
                val patIt = pat.expand
                if (patIt.nonEmpty) pq += refP -> patIt.map(Left(_))
                putBack()
                advance()

              case Suspend(refP) =>
                pq -= refP
                putBack()
                advance()

              case Seq(pat) =>
                val patIt = pat.expand
                val cat   = patIt.map(Left(_)) ++ it
                if (patIt.nonEmpty) {
                  pq += ref -> cat
                }
                advance()

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

    def reset(): Unit = ???

    def next(): Out = {
      if (done) throw new NoSuchElementException("next on empty iterator")
      val res = elem
      advance()
      res
    }
  }
}