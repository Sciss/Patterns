/*
 *  Combinations.scala
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
import de.sciss.patterns.stream.CombinationsImpl

final case class Combinations[A](in: Pat[A], n: Pat[Int]) extends Pattern[Pat[A]] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] =
    CombinationsImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
    val inT = t(in)
    val nT  = t(n)
    if (inT.eq(in) && nT.eq(n)) this else copy(in = inT, n = nT)
  }
}
