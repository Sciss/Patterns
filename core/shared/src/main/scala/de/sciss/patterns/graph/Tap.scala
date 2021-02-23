/*
 *  Tap.scala
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
import de.sciss.patterns.stream.TapImpl

final case class Tap[A, A1](in: Pat[A], side: Pat[A1]) extends Pattern[A] {
  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, A] =
    TapImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[A] = {
    val inT   = t(in)
    val sideT = t(side)
    if (inT.eq(in) && sideT.eq(side)) this else copy(in = inT, side = sideT)
  }
}