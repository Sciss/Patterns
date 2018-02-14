/*
 *  Sliding.scala
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

package de.sciss.patterns.graph

import de.sciss.patterns.{Context, Pat, Pattern, Stream}

final case class Sliding[A](in: Pat[A], size: Pat[Int], step: Pat[Int]) extends Pattern[Pat[A]] { pat =>

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, Pat[A]] = new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, Pat[A]] {
    private[this] val inStream    = pat.in  .expand(ctx, tx0)
    private[this] val sizeStream  = pat.size.expand(ctx, tx0)
    private[this] val stepStream  = pat.step.expand(ctx, tx0)

    private[this] val innerStream = ctx.newVar[Pat[A]](null) // Stream[Tx, A]](null)
    private[this] val _valid      = ctx.newVar(false)
    private[this] val _hasNext    = ctx.newVar(false)
    private[this] val _hasStep    = ctx.newVar(true)
    private[this] val _buf        = ctx.newVar[Vector[A]](null)

    def reset()(implicit tx: Tx): Unit =
      _valid() = false

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        _hasStep()  = true
        _buf()      = Vector.empty
        advance()
      }

    private def advance()(implicit tx: Tx): Unit = {
      val shn     = _hasStep() && sizeStream.hasNext && inStream.hasNext
      _hasNext()  = shn
      if (shn) {
        val sizeVal = math.max(0, sizeStream.next())
        val b       = Vector.newBuilder[A]
        b.sizeHint(sizeVal)
        val vecOld  = _buf()
        var i = 0
        while (i < sizeVal && i < vecOld.size) {
          b += vecOld(i)
          i += 1
        }
        while (i < sizeVal && inStream.hasNext) {
          b += inStream.next()
          i += 1
        }
        val vecNew    = b.result()
        val inner     = Pat(vecNew: _*) // Stream[Tx, A](vecNew: _*)
        innerStream() = inner
        val ihn       = sizeVal > 0
        _hasNext()    = ihn
        if (ihn) {
          val phn     = stepStream.hasNext
          _hasStep()  = phn
          if (phn) {
            val stepVal = math.max(0, stepStream.next())
            _buf()      = vecNew.drop(stepVal)
            while (i < stepVal && inStream.hasNext) {
              inStream.next()
              i += 1
            }
            _hasStep()  = stepVal > 0
          }
        }
      }
    }

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): Pat[A] = {
      validate()
      if (!_hasNext()) Stream.exhausted()
      val res = innerStream()
      advance()
      res
    }
  }
}