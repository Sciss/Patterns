/*
 *  Apply.scala
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

final case class Apply[A](in: Pat[Pat[A]], idx: Pat[Int]) extends Pattern[A] {
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    val res = new StreamImpl[Tx](tx)
//    println(s"NEW APPLY STREAM ${res.hashCode().toHexString}")
//    (new Exception).fillInStackTrace().printStackTrace()
    res
  }

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT   = t(in)
    val idxT  = t(idx)
    if (inT.eq(in) && idxT.eq(idx)) this else copy(in = inT, idx = idxT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx])
    extends Stream[Tx, A] {

    private[this] val inStream  = in  .expand(ctx, tx0)
    private[this] val idxStream = idx .expand(ctx, tx0)

    private[this] val _valid    = ctx.newVar(false)(tx0)
    private[this] val _hasNext  = ctx.newVar(false)(tx0)
    private[this] val state     = ctx.newVar[Stream[Tx, A]](null)(tx0)

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      idxStream .reset()
    }

    private def validate()(implicit tx: Tx): Unit = if (!_valid()) {
      _valid() = true
      val xhn = idxStream.hasNext
      if (xhn) {
        val idxVal = idxStream.next()
        if (idxVal >= 0) {
          var i = 0
          while (i < idxVal && inStream.hasNext) {
            inStream.next()
            i += 1
          }
          if (inStream.hasNext) {
            val inVal   = inStream.next()
            val _state  = inVal.expand[Tx]
            state()     = _state
            _hasNext()  = _state.hasNext
          }

        } else {
          _hasNext() = false
        }

      } else {
        _hasNext() = false
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val _state  = state()
      val res     = _state.next()
      if (!_state.hasNext) _hasNext() = false
      res
    }
  }
}
