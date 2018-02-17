/*
 *  Pseq.scala
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

final case class Pseq[A](list: Seq[Pat[A]], repeats: Pat[Int] = 1, offset : Pat[Int] = 0)
  extends Pattern[A] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  def transform(t: Transform): Pat[A] = {
    val listT     = list.map(t(_))
    val repeatsT  = t(repeats)
    val offsetT   = t(offset)
    copy(list = listT, repeats = repeatsT, offset = offsetT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val repeatsIt   = repeats.expand(ctx, tx0)
    private[this] val offsetIt    = offset .expand(ctx, tx0)
    private[this] val indexed     = list.toIndexedSeq
    private[this] val sizeVal     = indexed.size

    private[this] val repeatsVal  = ctx.newVar(0)
    private[this] val offsetVal   = ctx.newVar(0)

    private[this] val listIt      = ctx.newVar[Stream[Tx, A]](null)

    private[this] val repeatsCnt  = ctx.newVar(0)
    private[this] val sizeCnt     = ctx.newVar(0)

    private[this] val _hasNext    = ctx.newVar(false)
    private[this] val _valid      = ctx.newVar(false)

    private def mkListIter()(implicit tx: Tx): Stream[Tx, A] = {
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
            listIt() = mkListIter()
          }
        }
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): A = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = listIt().next()
      if (listIt().isEmpty) {
        sizeCnt() = sizeCnt() + 1
        if (sizeCnt() == sizeVal) {
          repeatsCnt()  = repeatsCnt() + 1
          sizeCnt()     = 0
          _hasNext()    = repeatsCnt() < repeatsVal()
        }
        if (_hasNext()) listIt() = mkListIter()
      }
      res
    }
  }
}
