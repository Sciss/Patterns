/*
 *  Poll.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.lucre.Exec
import de.sciss.patterns.stream.PollImpl

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

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    PollImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val inT     = t(in)
    val gateT   = t(gate)
    val labelT  = t(label)
    if (inT.eq(in) && gateT.eq(gate) && labelT.eq(label)) this else copy(in = inT, gate = gateT, label = labelT)
  }
}