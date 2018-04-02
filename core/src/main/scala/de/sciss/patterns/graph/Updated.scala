/*
 *  Updated.scala
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

final case class Updated[A1, A >: A1](in: Pat[A1], idx: Pat[Int], elem: A) extends Pattern[A] {
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT   = t(in)
    val idxT  = t(idx)
    if (inT.eq(in) && idxT.eq(idx)) this else copy(in = inT, idx = idxT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, A] {

    private[this] val inStream    = in  .expand(ctx, tx0)
    private[this] val idxStream   = idx .expand(ctx, tx0)

    private[this] val _valid      = ctx.newVar(false)(tx0)
    private[this] val _hasNext    = ctx.newVar(false)(tx0)
    private[this] val takeRem     = ctx.newVar(0)(tx0)

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      idxStream .reset()
    }

    private def validate()(implicit tx: Tx): Unit = if (!_valid()) {
      _valid()  = true
      val xhn   = idxStream.hasNext
      if (xhn) {
        val _idxVal = idxStream.next()
        takeRem()   = _idxVal
        _hasNext()  = _idxVal >= 0 && inStream.hasNext
      } else {
        _hasNext()  = false
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val c     = takeRem()
      val inVal = inStream.next()
      val res   = if (c == 0) elem else inVal
      if (c > 0) takeRem() = c - 1
      if (!inStream.hasNext) _hasNext() = false
      res
    }
  }
}
