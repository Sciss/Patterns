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
import de.sciss.patterns.Types.{IntLikeTop, IntTop, Top}

import scala.collection.AbstractIterator

//final case class Index[A <: Value](list: PE[A], index: PE.Int, repeats: PE.Int = 1) extends Pattern[A] {
//  def iterator(implicit b: StreamGraph.Builder): Stream[A] = ???
//}

final case class Pseq[T1 <: Top, T2 <: IntLikeTop, T <: Top](list   : Seq[Pat[T1]],
                                                             repeats: Pat.Int = 1,
                                                             offset : Pat[T2] = 0)
                                                            (implicit val tpe: T { type Out = T2#Index[T1#Out] })
  extends Pattern[T { type Out = T2#Index[T1#Out] }] {

  type Out = T2#Index[T1#Out]

  def iterator(implicit ctx: Context): Iterator[Out] = {
    val repeatsIt = repeats.expand
    val offsetIt  = offset .expand
    val indexed   = list.toIndexedSeq
    val sizeVal   = indexed.size
    if (repeatsIt.isEmpty || offsetIt.isEmpty || sizeVal == 0) Iterator.empty
    else {
      val repeatsVal  = repeatsIt.next()
      val offsetVal   = offsetIt .next()

      new AbstractIterator[Out] {
        private[this] var repeatsCnt  = 0
        private[this] var sizeCnt     = 0

        private def mkListIter(): Iterator[Out] = {
          import IntFunctions.wrap
          import offset.tpe.{Index, mapIndex, traverseIndex}
          val itx: Index[Iterator[T1#Out]] = mapIndex(offsetVal) { off0 =>
            val i             = wrap(sizeCnt + off0, 0, sizeVal - 1)
            val elem: Pat[T1] = indexed(i)
            elem.embed
          }
          val it: Iterator[Index[T1#Out]] = traverseIndex[Iterator, Iterator[T1#Out], T1#Out](itx)(identity)
          it
        }

        private[this] var listIter: Iterator[Out] = mkListIter()

        def hasNext: Boolean = repeatsCnt < repeatsVal

        def next(): T2#Index[T1#Out] = {
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
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Shuf(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Rand(list: PE, length: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class XRand(list: PE, length: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class WRand(list: PE, weights: PE, length: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class FSM(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  private[patterns] def makeStream(args: Vec[StreamIn]) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Switch(list: PE, index: PE = 0) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}

/** aka Ptuple */
final case class Zip[T <: Top](list: Seq[Pat[T]], repeats: Pat.Int = 1)(implicit peerTpe: T)
  extends Pattern[Top.Seq[T]] {

  val tpe: Top.Seq[T] = Top.Seq[T]

  def iterator(implicit ctx: Context): Iterator[tpe.Out] = {
    ???
  }
}

//final case class Lace(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Slide(list: PE, repeats: PE = 1, size: PE = 3, step: PE = 1, start: PE = 0, wrap: Boolean = true)
//  extends Pattern {
//
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}
//
//final case class Walk(list: PE, step: PE, dir: PE = 1, start: PE = 0)
//  extends Pattern {
//
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ???
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ???
//}