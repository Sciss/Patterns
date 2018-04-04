/*
 *  Choose.scala
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

final case class Choose[A](in: Pat[A]) extends Pattern[A] { pat =>
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val id        = tx0.newId()
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val _valid    = tx0.newBooleanVar(id, false)
    private[this] val _hasNext  = tx0.newBooleanVar(id, false)
    private[this] val choice    = ??? : S#Var[A] // ctx.newVar[A](null.asInstanceOf[A])(tx0)

    private[this] implicit val r: Random[S#Tx] = ctx.mkRandom(pat.ref)(tx0)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid()  = true
        val vec   = inStream.toVector
        if (vec.nonEmpty) {
          val idx     = r.nextInt(vec.size)
          choice()    = vec(idx)
          _hasNext()  = true
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
      val res = choice()
      _hasNext() = false
      res
    }
  }
}