/*
 *  MapWithIndex.scala
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
import de.sciss.patterns.stream.MapWithIndexImpl

final case class MapWithIndex[A1, A] private[patterns](outer: Pat[Pat[A1]], itIn: It[A1], itIdx: It[Int], inner: Pat[A])
  extends Pattern[Pat[A]] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, Pat[A]] =
    MapWithIndexImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[Pat[A]] = {
    val outerT  = t(outer)
    val innerT  = t(inner)
    if (outerT.eq(outer) && innerT.eq(inner)) this else {
      val (itT, innerT1) = itIn.replaceIn(innerT)
      copy(outer = outerT, itIn = itT, inner = innerT1)
    }
  }
}
