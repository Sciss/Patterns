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

final case class Choose[A](in: Pat[A]) extends Pattern[A] { pat =>
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val _valid    = ctx.newVar(false)(tx0)
    private[this] val _hasNext  = ctx.newVar(false)(tx0)
    private[this] val choice    = ctx.newVar[A](null.asInstanceOf[A])(tx0)

    private[this] implicit val r: Random[Tx] = ctx.mkRandom(pat.ref)(tx0)

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    private def validate()(implicit tx: Tx): Unit =
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

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = choice()
      _hasNext() = false
      res
    }
  }
}