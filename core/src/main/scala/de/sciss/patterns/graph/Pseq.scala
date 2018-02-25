///*
// *  Pseq.scala
// *  (Patterns)
// *
// *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU General Public License v2+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.patterns
//package graph
//
//import de.sciss.numbers.IntFunctions
//
//final case class Pseq[A](list: Seq[Pat[A]], repeats: Pat[Int] = 1, offset : Pat[Int] = 0)
//  extends Pattern[A] {
//
//  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)
//
//  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
//    val listT     = list.map(t(_))
//    val repeatsT  = t(repeats)
//    val offsetT   = t(offset)
//    copy(list = listT, repeats = repeatsT, offset = offsetT)
//  }
//
//  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
//    private[this] val repeatsStream = repeats.expand(ctx, tx0)
//    private[this] val offsetStream  = offset .expand(ctx, tx0)
//
//    private[this] val indexed       = list.toIndexedSeq
//    private[this] val sizeVal       = indexed.size
//
//    private[this] val repeatsVal    = ctx.newVar(0)
//    private[this] val offsetVal     = ctx.newVar(0)
//
//    private[this] val listIt        = ctx.newVar[Stream[Tx, A]](null)
//
//    private[this] val repeatsCnt    = ctx.newVar(0)
//    private[this] val sizeCnt       = ctx.newVar(0)
//
//    private[this] val _hasNext      = ctx.newVar(false)
//    private[this] val _valid        = ctx.newVar(false)
//
//    private def mkListIter()(implicit tx: Tx): Stream[Tx, A] = {
//      import IntFunctions.wrap
//      val i = wrap(sizeCnt() + offsetVal(), 0, sizeVal - 1)
//      indexed(i).expand
//    }
//
//    def reset()(implicit tx: Tx): Unit =
//      if (_valid()) {
//        _valid() = false
//        repeatsStream .reset()
//        offsetStream  .reset()
//      }
//
//    private def validate()(implicit tx: Tx): Unit =
//      if (!_valid()) {
//        _valid()      = true
//        repeatsCnt()  = 0
//        sizeCnt()     = 0
//        _hasNext()    = repeatsStream.hasNext && offsetStream.hasNext
//        if (_hasNext()) {
//          repeatsVal() = repeatsStream.next()
//          offsetVal () = offsetStream .next()
//          _hasNext() = repeatsCnt() < repeatsVal()
//          if (_hasNext()) {
//            listIt() = mkListIter()
//          }
//        }
//      }
//
//    def hasNext(implicit tx: Tx): Boolean = {
//      validate()
//      _hasNext()
//    }
//
//    def next()(implicit tx: Tx): A = {
//      if (!hasNext) Stream.exhausted()
//      val res = listIt().next()
//      if (listIt().isEmpty) {
//        sizeCnt() = sizeCnt() + 1
//        if (sizeCnt() == sizeVal) {
//          repeatsCnt()  = repeatsCnt() + 1
//          sizeCnt()     = 0
//          _hasNext()    = repeatsCnt() < repeatsVal()
//        }
//        if (_hasNext()) listIt() = mkListIter()
//      }
//      res
//    }
//  }
//}
