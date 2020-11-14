/*
 *  Apply.scala
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
import de.sciss.patterns.stream.ApplyImpl

final case class Apply[A](in: Pat[Pat[A]], idx: Pat[Int]) extends Pattern[A] {
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    ApplyImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val inT   = t(in)
    val idxT  = t(idx)
    if (inT.eq(in) && idxT.eq(idx)) this else copy(in = inT, idx = idxT)
  }
}
