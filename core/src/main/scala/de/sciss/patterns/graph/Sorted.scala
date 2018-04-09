/*
 *  Sorted.scala
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
import de.sciss.patterns.Types.{Aux, ScalarOrd}
import de.sciss.patterns.stream.SortedImpl

final case class Sorted[A](in: Pat[A])(implicit val ord: ScalarOrd[A]) extends Pattern[A] {
  override private[patterns] def aux: List[Aux] = ord :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    SortedImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT = t(in)
    if (inT eq in) this else copy(in = inT)
  }
}
