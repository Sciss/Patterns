/*
 *  Differentiate.scala
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
import de.sciss.patterns.Types.{Aux, Num}
import de.sciss.patterns.impl.PatElem

final case class Differentiate[A](in: Pat[A])(implicit num: Num[A]) extends Pattern[A] {
  override private[patterns] def aux: List[Aux] = num :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val id        = tx0.newId()
    private[this] val inStream  = in.expand(ctx, tx0)
    private[this] val _valid    = tx0.newBooleanVar(id, false)
    private[this] val _hasNext  = tx0.newBooleanVar(id, false)
    private[this] val x1        = PatElem.makeVar[S, A](id)(tx0)
    private[this] val state     = PatElem.makeVar[S, A](id)(tx0)

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream.reset()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid() = true
        if (inStream.hasNext) {
          x1() = inStream.next()
          advance()
        } else {
          _hasNext() = false
        }
      }

    private def advance()(implicit tx: S#Tx): Unit = {
      if (inStream.hasNext) {
        val in1     = x1()
        val in0     = inStream.next()
        x1()        = in0
        state()     = num.-(in0, in1)
        _hasNext()  = true
      } else {
        _hasNext() = false
      }
    }

    def hasNext(implicit ctx: Context[S], tx: S#Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit ctx: Context[S], tx: S#Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      advance()
      res
    }
  }
}