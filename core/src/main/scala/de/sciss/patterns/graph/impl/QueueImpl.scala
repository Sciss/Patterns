/*
 *  QueueImpl.scala
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
package impl

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap => ISortedMap}

object QueueImpl {

  sealed trait Cmd
  final case class Par    (ref: TimeRef, pat: Pat.Event)  extends Cmd
  final case class Suspend(ref: TimeRef)                  extends Cmd
  final case class Seq    (pat: Pat.Event)                extends Cmd
  final case class Advance(seconds: Double)               extends Cmd

//  sealed trait Blocking
//  final class SeqB(val it: Iterator[Event#Out]) extends Blocking {
//    var done: Boolean = it.hasNext
//    var curr: Event#Out = if (done) null else it.next()
//  }
//  final case class AdvanceB(stop: Double) extends Blocking
//  final case object Flush extends Blocking
}
final class QueueImpl[Tx](tx0: Tx)(implicit val context: Context[Tx])
  extends Spawner.Queue[Tx] {

  import QueueImpl._

  type Ref = TimeRef

  @transient final private[this] lazy val _ref = new AnyRef

  private[this] val refCnt  = context.newVar(0)
  private[this] val cmdRev  = context.newVar(List.empty[Cmd])

  implicit val random: Random[Tx] = context.mkRandom(_ref)(tx0)

  private def mkRef()(implicit tx: Tx): Ref = {
    val c = refCnt()
    val ref = new Ref(c)
    refCnt() = c + 1
    ref
  }

  def par(pat: Pat.Event)(implicit tx: Tx): Ref = {
    val ref = mkRef()
    cmdRev() = Par(ref, pat) :: cmdRev()
    ref
  }

  def seq(pat: Pat.Event)(implicit tx: Tx): Unit =
    cmdRev() = Seq(pat) :: cmdRev()

  def suspend(ref: Ref)(implicit tx: Tx): Unit =
    cmdRev() = Suspend(ref) :: cmdRev()

  def advance(seconds: Double)(implicit tx: Tx): Unit =
    cmdRev() = Advance(seconds) :: cmdRev()

  type Out = Event#Out[Tx]

  def iterator: Stream[Tx, Out] = new Stream[Tx, Out] {
    private[this] val pq = context.newVar[ISortedMap[Ref, Stream[Tx, Either[Out, Cmd]]]](null)

//    private[this] var cmdRef = Map.empty[Ref, Ref]

//    private[this] var now       = 0.0
    private[this] val elem      = context.newVar[Out](null)
    private[this] val _hasNext  = context.newVar(false)

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
    private def advance()(implicit tx: Tx): Unit = {
      if (pq().nonEmpty) {
        val (ref, it) = pq().head
        pq()          = pq().tail

        def putBack(): Unit =
          if (it.hasNext) {
            pq() = pq() + (ref -> it)
          }

        it.next() match {
          case Left(_elem) =>
            elem()    = _elem
            val d     = math.max(0.0, Event.delta(_elem))
            val now   = ref.time
            ref.time += d
            putBack()
            if (pq().nonEmpty) {
              val nextTime = pq().firstKey.time
              elem() = elem() + (Event.keyDelta -> (nextTime - now))
            }

          case Right(cmd) =>
            cmd match {
              case Par(refP, pat) =>
                refP.time = ref.time // now
                val patIt: Stream[Tx, Event#Out[Tx]] = pat.expand
                if (patIt.nonEmpty) pq() = pq() + (refP -> patIt.map(Left(_)))
                putBack()
                advance()

              case Suspend(refP) =>
                pq() = pq() - refP
                putBack()
                advance()

              case Seq(pat) =>
                val patIt = pat.expand
                val cat   = patIt.map(Left(_)) ++ it
                if (patIt.nonEmpty) {
                  pq() = pq() + (ref -> cat)
                }
                advance()

              case Advance(d) =>
                ref.time += d
                putBack()
                advance()
            }
        }

      } else {
        _hasNext() = false
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    private[this] val _valid = context.newVar(false)

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        val pqVal: ISortedMap[Ref, Stream[Tx, Either[Out, Cmd]]] =
          if (cmdRev().isEmpty) ISortedMap.empty else ISortedMap(mkRef() -> ???) // Stream.reverseIterator(cmdRev()).map(Right(_)))
        pq() = pqVal
        _valid() = true
        advance()
      }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    def next()(implicit tx: Tx): Out = {
      validate()
      if (!_valid()) Stream.exhausted()
      val res = elem()
      advance()
      res
    }
  }
}