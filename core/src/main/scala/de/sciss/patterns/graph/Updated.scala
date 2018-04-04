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

import de.sciss.lucre.stm.Base

final case class Updated[A1, A >: A1](in: Pat[A1], idx: Pat[Int], elem: A) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl[S](tx)

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
    private[this] val takeRem     = tx0.newIntVar(id, 0)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      idxStream .reset()
    }

    private def validate()(implicit tx: S#Tx): Unit = if (!_valid()) {
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

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: S#Tx): A = {
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
