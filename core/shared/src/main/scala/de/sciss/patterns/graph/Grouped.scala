/*
 *  Grouped.scala
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
import de.sciss.patterns.stream.GroupedImpl

final case class Grouped[A](in: Pat[A], size: Pat[Int]) extends Pattern[Pat[A]] { pat =>
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] =
    GroupedImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Pat[A]] = {
    val inT   = t(in)
    val sizeT = t(size)
    if (inT.eq(in) && sizeT.eq(size)) this else copy(in = inT, size = sizeT)
  }
}
