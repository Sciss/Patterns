/*
 *  Hold.scala
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
import de.sciss.patterns.PatImport._
import de.sciss.patterns.stream.HoldImpl

/** A pattern that holds (and repeats) input values whenever `hold` is true.
  * With the default of constant `true`, the pattern will repeat the first
  * input element forever.
  */
final case class Hold[A](in: Pat[A], hold: Pat[Boolean] = true) extends Pattern[A] {
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    HoldImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val inT   = t(in)
    val holdT = t(hold)
    if (inT.eq(in) && holdT.eq(hold)) this else copy(in = inT, hold = holdT)
  }
}
