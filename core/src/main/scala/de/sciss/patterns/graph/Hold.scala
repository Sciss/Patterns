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

/** A pattern that holds (and repeats) input values whenever `hold` is true.
  * With the default of constant `true`, the pattern will repeat the first
  * input element forever.
  */
final case class Hold[A](in: Pat[A], hold: Pat[Boolean] = true) extends Pattern[A] {
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = new StreamImpl(tx)

  def transform(t: Transform): Pat[A] = {
    val inT   = t(in)
    val holdT = t(hold)
    if (inT.eq(in) && holdT.eq(hold)) this else copy(in = inT, hold = holdT)
  }

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, A] {
    private[this] val inStream    = in  .expand(ctx, tx0)
    private[this] val holdStream  = hold.expand(ctx, tx0)

    private[this] val _valid      = ctx.newVar(false)
    private[this] val _hasNext    = ctx.newVar(false)
    private[this] val _hasIn      = ctx.newVar(false)
    private[this] val state       = ctx.newVar[A](null.asInstanceOf[A])

    def reset()(implicit tx: Tx): Unit =
      if (_valid()) {
        _valid() = false
        inStream  .reset()
        holdStream.reset()
      }

    private def validate()(implicit tx: Tx): Unit =
      if (!_valid()) {
        _valid()  = true
        _hasIn()  = false
        advance()
      }

    private def advance()(implicit tx: Tx): Unit = {
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

    def hasNext(implicit tx: Tx): Boolean = {
      validate()
      _hasNext()
    }

    def next()(implicit tx: Tx): A = {
      if (!hasNext) Stream.exhausted()
      val res = state()
      advance()
      res
    }
  }
}
