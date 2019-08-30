/*
 *  UpdatedAll.scala
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
import de.sciss.patterns.stream.UpdatedAllImpl

final case class UpdatedAll[A1, A >: A1](in: Pat[A1], idx: Pat[Int], elem: Pat[A]) extends Pattern[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    UpdatedAllImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT   = t(in)
    val idxT  = t(idx)
    val elemT = t(elem)
    if (inT.eq(in) && idxT.eq(idx) && elemT.eq(elem)) this else copy(in = inT, idx = idxT, elem = elemT)
  }
}
