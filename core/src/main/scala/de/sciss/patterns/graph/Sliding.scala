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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.impl.PatElem
import de.sciss.patterns.{Context, Pat, Pattern, Stream, Transform}

import scala.collection.immutable.{IndexedSeq => Vec}

final case class Sliding[A](in: Pat[A], size: Pat[Int], step: Pat[Int]) extends Pattern[Pat[A]] { pat =>

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
    val inT   = t(in)
    val sizeT = t(size)
    val stepT = t(step)
    if (inT.eq(in) && sizeT.eq(size) && stepT.eq(step)) this else copy(in = inT, size = sizeT, step = stepT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, Pat[A]] {
    private[this] val id          = tx0.newId()
    private[this] val inStream    = pat.in  .expand(ctx, tx0)
    private[this] val sizeStream  = pat.size.expand(ctx, tx0)
    private[this] val stepStream  = pat.step.expand(ctx, tx0)

    private[this] val innerStream = tx0.newVar[Pat[A]](id, null)
    private[this] val _valid      = tx0.newBooleanVar(id, false)
    private[this] val _hasNext    = tx0.newBooleanVar(id, false)
    private[this] val _hasStep    = tx0.newBooleanVar(id, true)
    private[this] val _buf        = tx0.newVar[Vec[A]](id, Vector.empty)(PatElem.vecSerializer)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      sizeStream.reset()
      stepStream.reset()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid()    = true
        _hasStep()  = true
        _buf()      = Vector.empty
        advance()
      }

    private def advance()(implicit tx: S#Tx): Unit = {
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
        val inner     = Pat(vecNew: _*) // Stream[S, A](vecNew: _*)
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

    def hasNext(implicit tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: S#Tx): Pat[A] = {
      if (!hasNext) Stream.exhausted()
      val res = innerStream()
      advance()
      res
    }
  }
}