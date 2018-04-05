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

import de.sciss.lucre.stm.Base

final case class Apply[A](in: Pat[Pat[A]], idx: Pat[Int]) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    val res = new StreamImpl[S](tx)
//    println(s"NEW APPLY STREAM ${res.hashCode().toHexString}")
//    (new Exception).fillInStackTrace().printStackTrace()
    res
  }

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT   = t(in)
    val idxT  = t(idx)
    if (inT.eq(in) && idxT.eq(idx)) this else copy(in = inT, idx = idxT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S])
    extends Stream[S, A] {

    private[this] val id          = tx0.newId()
    private[this] val inStream    = in  .expand(ctx, tx0)
    private[this] val idxStream   = idx .expand(ctx, tx0)
    private[this] val _valid      = tx0.newBooleanVar(id, false)
    private[this] val _hasNext    = tx0.newBooleanVar(id, false)
    private[this] val state       = tx0.newVar[Stream[S, A]](id, null)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      idxStream .reset()
    }

    private def validate()(implicit tx: S#Tx): Unit = if (!_valid()) {
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
            val _state  = inVal.expand[S]
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

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val _state  = state()
      val res     = _state.next()
      if (!_state.hasNext) _hasNext() = false
      res
    }
  }
}
