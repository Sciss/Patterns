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
import de.sciss.patterns.Types.Top

final case class Pseq[T <: Top](list: Seq[Pat[T]], repeats: Pat.Int = 1, offset : Pat.Int = 0)
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl(tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    private[this] val repeatsIt   = repeats.expand(ctx, tx0)
    private[this] val offsetIt    = offset .expand(ctx, tx0)
    private[this] val indexed     = list.toIndexedSeq
    private[this] val sizeVal     = indexed.size

    private[this] val repeatsVal  = ctx.newVar(0)
    private[this] val offsetVal   = ctx.newVar(0)

    private[this] val listIt      = ctx.newVar[Stream[Tx, T#Out[Tx]]](null)

    private[this] val repeatsCnt  = ctx.newVar(0)
    private[this] val sizeCnt     = ctx.newVar(0)

    private[this] val _hasNext    = ctx.newVar(false)
    private[this] val _valid      = ctx.newVar(false)

    private def mkListIter()(implicit tx: Tx): Stream[Tx, T#Out[Tx]] = {
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

    def next()(implicit tx: Tx): T#Out[Tx] = {
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
