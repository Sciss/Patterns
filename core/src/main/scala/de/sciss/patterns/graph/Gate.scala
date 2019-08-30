/*
 *  Gate.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.lucre.stm.Base
import de.sciss.patterns.stream.GateImpl

/** A pattern that filters elements on an `in` input pattern based on a `gate` predicate.
  * Whenever `gate` is `true`, the input element is passed to output, otherwise it is dropped.
  */
final case class Gate[A](in: Pat[A], gate: Pat[Boolean]) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    GateImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT   = t(in)
    val gateT = t(gate)
    if (inT.eq(in) && gateT.eq(gate)) this else copy(in = inT, gate = gateT)
  }
}
