/*
 *  Shuffle.scala
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

import scala.collection.immutable.{IndexedSeq => Vec}

final case class Shuffle[A](in: Pat[A]) extends Pattern[A] { pat =>
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl[Tx](tx)

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = {
    val inT = t(in)
    if (inT.eq(in)) this else copy(in = inT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val _valid    = ctx.newVar(false)(tx0)
    private[this] val _hasNext  = ctx.newVar(false)(tx0)
    private[this] val count     = ctx.newVar(0)(tx0)
    private[this] val shuffled  = ctx.newVar[Vec[A]](null)(tx0)

    private[this] implicit val r: Random[Tx] = ctx.mkRandom(pat.ref)(tx0)

    def reset()(implicit tx: Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()  = true
        count()   = 0
        var rem   = inStream.toVector
        val b     = Vector.newBuilder[A]
        b.sizeHint(rem.size)
        // XXX TODO --- this is probably not the fastest possible implementation
        while (rem.nonEmpty) {
          val idx = r.nextInt(rem.size)
          val e   = rem(idx)
          rem     = rem.patch(idx, Nil, 1)
          b      += e
        }
        val vec     = b.result()
        shuffled()  = vec
        _hasNext()  = vec.nonEmpty
      }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val vec = shuffled()
      val c   = count()
      val res = vec(c)
      val c1  = c + 1
      count() = c1
      if (c1 == vec.size) _hasNext() = false
      res
    }
  }
}