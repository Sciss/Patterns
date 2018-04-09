/*
 *  FlatTabulate.scala
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
import de.sciss.patterns.stream.LoopWithIndexImpl

final case class LoopWithIndex[A] private[patterns](n: Pat[Int], it: It[Int], inner: Pat[A] /* , innerLevel: Int */)
  extends Pattern[A] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    LoopWithIndexImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val nT      = t(n)
    val innerT  = t(inner)
    if (nT.eq(n) && innerT.eq(inner)) this else copy(n = nT, inner = innerT)
  }
}
