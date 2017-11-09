/*
 *  ListPatterns.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
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
import scala.collection.AbstractIterator

//final case class Index[A <: Value](list: PE[A], index: PE.Int, repeats: PE.Int = 1) extends Pattern[A] {
//  def iterator(implicit b: StreamGraph.Builder): Stream[A] = ...
//}

final case class Pseq[T <: Top](list: Seq[Pat[T]], repeats: Pat.Int = 1, offset : Pat.Int = 0)
  extends Pattern[T] {

//  type Out = T2#Index[T1#Out]

  def iterator(implicit ctx: Context): Iterator[T#Out] = {
    val repeatsIt = repeats.expand
    val offsetIt  = offset .expand
    val indexed   = list.toIndexedSeq
    val sizeVal   = indexed.size
    if (repeatsIt.isEmpty || offsetIt.isEmpty || sizeVal == 0) Iterator.empty
    else {
      val repeatsVal  = repeatsIt.next()
      val offsetVal   = offsetIt .next()

      new AbstractIterator[T#Out] {
        private[this] var repeatsCnt  = 0
        private[this] var sizeCnt     = 0

        private def mkListIter(): Iterator[T#Out] = {
          import IntFunctions.wrap
          val i = wrap(sizeCnt + offsetVal, 0, sizeVal - 1)
          indexed(i).embed
        }

        private[this] var listIter: Iterator[T#Out] = mkListIter()

        def hasNext: Boolean = repeatsCnt < repeatsVal

        def next(): T#Out /* T2#Index[T1#Out] */ = {
          val res = listIter.next()
          if (listIter.isEmpty) {
            sizeCnt += 1
            if (sizeCnt == sizeVal) {
              repeatsCnt += 1
              sizeCnt     = 0
            }
            if (hasNext) listIter = mkListIter()
          }
          res
        }
      }
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

  def iterator(implicit ctx: Context): Iterator[Seq[T#Out]] = {
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

  def iterator(implicit ctx: Context): Iterator[T#Out] = {
    val repeatsIt = repeats.expand
    val sizeIt    = size   .expand
    val stepIt    = step   .expand
    val startIt   = start  .expand
    val indexed   = list.toIndexedSeq
    val listSize  = indexed.size
    if (repeatsIt.isEmpty || sizeIt.isEmpty || stepIt.isEmpty || startIt.isEmpty || listSize == 0) Iterator.empty
    else {
      val repeatsVal  = repeatsIt.next()
      val startVal    = startIt  .next()

      new AbstractIterator[T#Out] {
        private[this] var pos           = startVal
        private[this] var sizeVal       = sizeIt.next()
        private[this] var stepVal       = stepIt.next()
        private[this] var repeatsCnt    = 0
        private[this] var sizeCnt       = 0
        private[this] var listIt: Iterator[T#Out] = _
        private[this] var elem: T#Out   = _
        private[this] var done          = false

        private def pickIt(): Boolean = {
          val i = pos + sizeCnt
          val j = if (wrap) IntFunctions.wrap(i, 0, listSize - 1) else i
          val ok = j >= 0 && j < listSize
          listIt = if (ok) indexed(j).embed else Iterator.empty
          ok
        }

        @inline
        private def run(): Unit =
          if (pickIt()) advance() else done = true

        @tailrec
        private def advance(): Unit =
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

        def hasNext: Boolean = !done

        run()

        def next(): T#Out = {
          if (done) throw new NoSuchElementException("next on empty iterator")
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