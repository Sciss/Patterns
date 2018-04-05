/*
 *  Apply.scala
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

final case class Apply[A](in: Pat[Pat[A]], idx: Pat[Int]) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    impl.ApplyImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT   = t(in)
    val idxT  = t(idx)
    if (inT.eq(in) && idxT.eq(idx)) this else copy(in = inT, idx = idxT)
  }
}
