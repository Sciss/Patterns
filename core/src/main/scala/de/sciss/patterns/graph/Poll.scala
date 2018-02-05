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

import de.sciss.patterns.Types.Top

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
final case class Poll[T <: Top](in: Pat[T], gate: Pat.Boolean, label: Pat.String = "poll")
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out[Tx]] = new StreamImpl[Tx](tx)

  private final class StreamImpl[Tx](tx0: Tx)(implicit ctx: Context[Tx]) extends Stream[Tx, T#Out[Tx]] {
    type A = T#Out[Tx]

    private[this] val inStream    = in    .expand(ctx, tx0)
    private[this] val gateStream  = gate  .expand(ctx, tx0)
    private[this] val labelStream = label .expand(ctx, tx0)

    def reset()(implicit tx: Tx): Unit = ()

    def hasNext(implicit tx: Tx): Boolean = inStream.hasNext

    def next()(implicit tx: Tx): A = {
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