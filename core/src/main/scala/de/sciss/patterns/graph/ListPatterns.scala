/*
 *  ListPatterns.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.numbers.IntFunctions
import de.sciss.patterns.Types.Top

import scala.annotation.tailrec

//final case class Index[A <: Value](list: PE[A], index: PE.Int, repeats: PE.Int = 1) extends Pattern[A] {
//  def iterator(implicit b: StreamGraph.Builder): Stream[A] = ...
//}

final case class Pseq[T <: Top](list: Seq[Pat[T]], repeats: Pat.Int = 1, offset : Pat.Int = 0)
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#TOut[Tx]] = new Stream[Tx, T#TOut[Tx]] {
    private[this] val repeatsIt   = repeats.expand[Tx]
    private[this] val offsetIt    = offset .expand[Tx]
    private[this] val indexed     = list.toIndexedSeq
    private[this] val sizeVal     = indexed.size

    private[this] val repeatsVal  = ctx.newVar(0)
    private[this] val offsetVal   = ctx.newVar(0)

    private[this] val listIter    = ctx.newVar[Stream[Tx, T#TOut[Tx]]](null)

    private[this] val repeatsCnt  = ctx.newVar(0)
    private[this] val sizeCnt     = ctx.newVar(0)

    private[this] val _hasNext  = ctx.newVar(false)
    private[this] val _valid    = ctx.newVar(false)

    private def mkListIter()(implicit tx: Tx): Stream[Tx, T#TOut[Tx]] = {
      import IntFunctions.wrap
      val i = wrap(sizeCnt() + offsetVal(), 0, sizeVal - 1)
      indexed(i).embed
    }

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()      = true
        repeatsCnt()  = 0
        sizeCnt()     = 0
        _hasNext()    = repeatsIt.hasNext && offsetIt.hasNext
        if (_hasNext()) {
          repeatsVal() = repeatsIt.next()
          offsetVal () = offsetIt .next()
          _hasNext() = repeatsCnt() < repeatsVal()
          if (_hasNext()) {
            listIter() = mkListIter()
          }
        }
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): T#TOut[Tx] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = listIter().next()
      if (listIter().isEmpty) {
        sizeCnt() = sizeCnt() + 1
        if (sizeCnt() == sizeVal) {
          repeatsCnt()  = repeatsCnt() + 1
          sizeCnt()     = 0
          _hasNext()    = repeatsCnt() < repeatsVal()
        }
        if (_hasNext()) listIter() = mkListIter()
      }
      res
    }
  }
}

//final case class Pser(list: PE, length: PE = 1, offset: PE = 0) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}
//
//final case class Shuf(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}
//
//final case class Rand(list: PE, length: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}
//
//final case class XRand(list: PE, length: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}
//
//final case class WRand(list: PE, weights: PE, length: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}
//
//final case class FSM(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}
//
//final case class Switch(list: PE, index: PE = 0) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}

/** aka Ptuple */
final case class Zip[T <: Top](list: Seq[Pat[T]], repeats: Pat.Int = 1)
  extends Pattern[Top.Seq[T]] {

//  val tpe: Top.Seq[T] = Top.Seq[T]

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, Seq[T#TOut[Tx]]] = {
    ???
  }
}

//final case class Lace(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}

final case class Slide[T <: Top](list: Seq[Pat[T]], repeats: Pat.Int = 1, size: Pat.Int = 3, step: Pat.Int = 1,
                                 start: Pat.Int = 0, wrap: Boolean = true)
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#TOut[Tx]] = {
    val repeatsIt = repeats.expand
    val sizeIt    = size   .expand
    val stepIt    = step   .expand
    val startIt   = start  .expand
    val indexed   = list.toIndexedSeq
    val listSize  = indexed.size
    if (??? /* repeatsIt.isEmpty || sizeIt.isEmpty || stepIt.isEmpty || startIt.isEmpty || listSize == 0 */) Stream.empty
    else {
      val repeatsVal  = ??? : Int // repeatsIt.next()
      val startVal    = ??? : Int // startIt  .next()

      new Stream[Tx, T#TOut[Tx]] {
        private[this] var pos           = startVal
        private[this] var sizeVal       = ??? : Int // sizeIt.next()
        private[this] var stepVal       = ??? : Int // stepIt.next()
        private[this] var repeatsCnt    = 0
        private[this] var sizeCnt       = 0
        private[this] var listIt: Stream[Tx, T#TOut[Tx]] = _
        private[this] var elem: T#TOut[Tx]   = _
        private[this] var done          = false

        def reset()(implicit tx: Tx): Unit = ???

        private def pickIt(): Boolean = {
          val i = pos + sizeCnt
          val j = if (wrap) IntFunctions.wrap(i, 0, listSize - 1) else i
          val ok = j >= 0 && j < listSize
          listIt = if (ok) indexed(j).embed else Stream.empty
          ok
        }

        @inline
        private def run()(implicit tx: Tx): Unit =
          if (pickIt()) advance() else done = true

        @tailrec
        private def advance()(implicit tx: Tx): Unit =
          if (listIt.hasNext) {
            elem = listIt.next()
          } else {
            sizeCnt += 1
            if (sizeCnt < sizeVal) {
              run()

            } else {
              repeatsCnt += 1
              if (repeatsCnt < repeatsVal && sizeIt.hasNext && stepIt.hasNext) {
                pos        += stepVal
                sizeVal     = sizeIt.next()
                sizeCnt     = 0
                stepVal     = stepIt.next()
                advance()

              } else {
                done = true
              }
            }
          }

        def hasNext(implicit tx: Tx): Boolean = {
          ???
          !done
        }

        ??? // run()

        def next()(implicit tx: Tx): T#TOut[Tx] = {
          ???
          if (done) Stream.exhausted()
          val res = elem
          advance()
          res
        }
      }
    }
  }
}

//final case class Walk(list: PE, step: PE, dir: PE = 1, start: PE = 0)
//  extends Pattern {
//
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}