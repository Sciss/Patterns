/*
 *  IndexOfSlice.scala
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
import de.sciss.patterns.stream.IndexOfSliceImpl

final case class IndexOfSlice[A1, A2](in: Pat[A1], sub: Pat[A2], from: Pat[Int]) extends Pattern[Int] {
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Int] =
    IndexOfSliceImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Int] = {
    val inT   = t(in)
    val subT  = t(sub)
    val fromT = t(from)
    if (inT.eq(in) && subT.eq(sub) && fromT.eq(from)) this else copy(in = inT, sub = subT, from = fromT)
  }
}