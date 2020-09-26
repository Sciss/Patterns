/*
 *  Combinations.scala
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
import de.sciss.patterns.stream.CombinationsImpl

final case class Combinations[A](in: Pat[A], n: Pat[Int]) extends Pattern[Pat[A]] {

  def expand[T <: Exec[T]](implicit ctx: Context[T], tx: T): Stream[T, Pat[A]] =
    CombinationsImpl.expand(this)

  def transform[T <: Exec[T]](t: Transform)(implicit ctx: Context[T], tx: T): Pat[Pat[A]] = {
    val inT = t(in)
    val nT  = t(n)
    if (inT.eq(in) && nT.eq(n)) this else copy(in = inT, n = nT)
  }
}
