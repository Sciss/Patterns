/*
 *  IndexOfSlice.scala
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

final case class IndexOfSlice[A1, A2](in: Pat[A1], sub: Pat[A2], from: Pat[Int]) extends Pattern[Int] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Int] =
    impl.IndexOfSliceImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Int] = {
    val inT   = t(in)
    val subT  = t(sub)
    val fromT = t(from)
    if (inT.eq(in) && subT.eq(sub) && fromT.eq(from)) this else copy(in = inT, sub = subT, from = fromT)
  }
}