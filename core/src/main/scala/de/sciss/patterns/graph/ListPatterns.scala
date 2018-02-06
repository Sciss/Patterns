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
//
//package de.sciss.patterns
//package graph
//
//import de.sciss.numbers.IntFunctions
//import de.sciss.patterns.Types.Top
//
//final case class Index[A <: Value](list: PE[A], index: PE.Int, repeats: PE.Int = 1) extends Pattern[A] {
//  def iterator(implicit b: StreamGraph.Builder): Stream[A] = ...
//}
//
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
//
//final case class Lace(list: PE, repeats: PE = 1) extends Pattern {
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}
//
//final case class Slide[T <: Top](list: Seq[Pat[T]], repeats: Pat.Int = 1, size: Pat.Int = 3, step: Pat.Int = 1,
//                                 start: Pat.Int = 0, wrap: Boolean = true)
//  extends Pattern[T] {
//
//  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out[Tx]] = {
//    val repeatsIt = repeats.expand
//    val sizeIt    = size   .expand
//    val stepIt    = step   .expand
//    val startIt   = start  .expand
//    val indexed   = list.toIndexedSeq
//    val listSize  = indexed.size
//    if (... /* repeatsIt.isEmpty || sizeIt.isEmpty || stepIt.isEmpty || startIt.isEmpty || listSize == 0 */) Stream.empty
//    else {
//      val repeatsVal  = ... : Int // repeatsIt.next()
//      val startVal    = ... : Int // startIt  .next()
//
//      new Stream[Tx, T#Out[Tx]] {
//        private[this] var pos           = startVal
//        private[this] var sizeVal       = ... : Int // sizeIt.next()
//        private[this] var stepVal       = ... : Int // stepIt.next()
//        private[this] var repeatsCnt    = 0
//        private[this] var sizeCnt       = 0
//        private[this] var listIt: Stream[Tx, T#Out[Tx]] = _
//        private[this] var elem: T#Out[Tx]   = _
//        private[this] var done          = false
//
//        def reset()(implicit tx: Tx): Unit = ...
//
//        private def pickIt(): Boolean = {
//          val i = pos + sizeCnt
//          val j = if (wrap) IntFunctions.wrap(i, 0, listSize - 1) else i
//          val ok = j >= 0 && j < listSize
//          listIt = if (ok) indexed(j).embed else Stream.empty
//          ok
//        }
//
//        @inline
//        private def run()(implicit tx: Tx): Unit =
//          if (pickIt()) advance() else done = true
//
//        @tailrec
//        private def advance()(implicit tx: Tx): Unit =
//          if (listIt.hasNext) {
//            elem = listIt.next()
//          } else {
//            sizeCnt += 1
//            if (sizeCnt < sizeVal) {
//              run()
//
//            } else {
//              repeatsCnt += 1
//              if (repeatsCnt < repeatsVal && sizeIt.hasNext && stepIt.hasNext) {
//                pos        += stepVal
//                sizeVal     = sizeIt.next()
//                sizeCnt     = 0
//                stepVal     = stepIt.next()
//                advance()
//
//              } else {
//                done = true
//              }
//            }
//          }
//
//        def hasNext(implicit tx: Tx): Boolean = {
//          ...
//          !done
//        }
//
//        ... // run()
//
//        def next()(implicit tx: Tx): T#Out[Tx] = {
//          ...
//          if (done) Stream.exhausted()
//          val res = elem
//          advance()
//          res
//        }
//      }
//    }
//  }
//}

//final case class Walk(list: PE, step: PE, dir: PE = 1, start: PE = 0)
//  extends Pattern {
//
//  protected def makeStream(args: Vec[StreamIn])(implicit b: StreamGraph.Builder) = ...
//
//  protected def makeStreams(implicit b: StreamGraph.Builder) = ...
//}