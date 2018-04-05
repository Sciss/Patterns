/*
 *  Hold.scala
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
import de.sciss.patterns.impl.PatElem
import de.sciss.serial.DataOutput

/** A pattern that holds (and repeats) input values whenever `hold` is true.
  * With the default of constant `true`, the pattern will repeat the first
  * input element forever.
  */
final case class Hold[A](in: Pat[A], hold: Pat[Boolean] = true) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl(tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT   = t(in)
    val holdT = t(hold)
    if (inT.eq(in) && holdT.eq(hold)) this else copy(in = inT, hold = holdT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val id          = tx0.newId()
    private[this] val inStream    = in  .expand(ctx, tx0)
    private[this] val holdStream  = hold.expand(ctx, tx0)
    private[this] val _valid      = tx0.newBooleanVar(id, false)
    private[this] val _hasNext    = tx0.newBooleanVar(id, false)
    private[this] val _hasIn      = tx0.newBooleanVar(id, false)
    private[this] val state       = PatElem.makeVar[S, A](id)(tx0)

    protected def typeId: Int = ???

    protected def writeData(out: DataOutput): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???

    def reset()(implicit tx: S#Tx): Unit = if (_valid()) {
      _valid() = false
      inStream  .reset()
      holdStream.reset()
    }

    private def validate()(implicit tx: S#Tx): Unit =
      if (!_valid()) {
        _valid()  = true
        _hasIn()  = false
        advance()
      }

    private def advance()(implicit tx: S#Tx): Unit = {
      val hhn     = holdStream.hasNext
      _hasNext()  = hhn
      if (hhn) {
        val holdVal = holdStream.next()
        if (!holdVal) _hasIn() = false  // 'flush'

        if (!_hasIn()) {
          val ihn = inStream.hasNext
          if (ihn) {
            _hasIn()    = true
            val inVal   = inStream.next()
            state()     = inVal
          } else {
            _hasNext()  = false
          }
        }
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
