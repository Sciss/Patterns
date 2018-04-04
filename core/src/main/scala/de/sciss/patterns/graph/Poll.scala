/*
 *  Poll.scala
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

/** A pattern that prints snapshots of its input to the console.
  * The pattern passes its input through to the output.
  *
  * @param in     the input to be pulled. If this is a constant,
  *               the UGen will close after polling it. This is to
  *               prevent a dangling `Poll` whose trigger is
  *               infinite (such as `Impulse`). If you want to avoid
  *               that, you should wrap the input in a `DC`.
  * @param gate   a signal that whenever `high` causes the pattern
  *               to print the corresponding value of the input.
  * @param label  an identifying label to prepend to the printing.
  */
final case class Poll[A](in: Pat[A], gate: Pat[Boolean], label: Pat[String] = "poll")
  extends Pattern[A] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = new StreamImpl[S](tx)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT     = t(in)
    val gateT   = t(gate)
    val labelT  = t(label)
    if (inT.eq(in) && gateT.eq(gate) && labelT.eq(label)) this else copy(in = inT, gate = gateT, label = labelT)
  }

  private final class StreamImpl[S <: Base[S]](tx0: S#Tx)(implicit ctx: Context[S]) extends Stream[S, A] {
    private[this] val inStream    = in    .expand(ctx, tx0)
    private[this] val gateStream  = gate  .expand(ctx, tx0)
    private[this] val labelStream = label .expand(ctx, tx0)

    def reset()(implicit tx: S#Tx): Unit = {
      inStream    .reset()
      gateStream  .reset()
      labelStream .reset()
    }

    def hasNext(implicit tx: S#Tx): Boolean = inStream.hasNext

    def next()(implicit tx: S#Tx): A = {
      val res = inStream.next()
      if (gateStream.hasNext && labelStream.hasNext) {
        val gateValue   = gateStream  .next()
        val labelValue  = labelStream .next()
        if (gateValue) {
          println(s"$labelValue: $res")
        }
      }
      res
    }
  }
}