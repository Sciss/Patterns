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
import de.sciss.patterns.stream.HoldImpl

/** A pattern that holds (and repeats) input values whenever `hold` is true.
  * With the default of constant `true`, the pattern will repeat the first
  * input element forever.
  */
final case class Hold[A](in: Pat[A], hold: Pat[Boolean] = true) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    HoldImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT   = t(in)
    val holdT = t(hold)
    if (inT.eq(in) && holdT.eq(hold)) this else copy(in = inT, hold = holdT)
  }
}
