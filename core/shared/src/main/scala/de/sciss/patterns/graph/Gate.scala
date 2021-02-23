/*
 *  Gate.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2021 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.patterns.stream.GateImpl

/** A pattern that filters elements on an `in` input pattern based on a `gate` predicate.
  * Whenever `gate` is `true`, the input element is passed to output, otherwise it is dropped.
  */
final case class Gate[A](in: Pat[A], gate: Pat[Boolean]) extends Pattern[A] {
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    GateImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val inT   = t(in)
    val gateT = t(gate)
    if (inT.eq(in) && gateT.eq(gate)) this else copy(in = inT, gate = gateT)
  }
}
