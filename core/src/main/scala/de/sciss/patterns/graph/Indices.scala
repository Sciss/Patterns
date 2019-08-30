/*
 *  Indices.scala
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
import de.sciss.patterns.stream.IndicesImpl

case class Indices[A](in: Pat[A]) extends Pattern[Int] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] =
    IndicesImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Int] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }
}
