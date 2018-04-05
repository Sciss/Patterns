/*
 *  Brown.scala
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
import de.sciss.patterns.Types.{Aux, Num, Widen2}

final case class Brown[A1, A2, A](lo: Pat[A1], hi: Pat[A1], step: Pat[A2])
                                 (implicit val widen: Widen2[A1, A2, A], val num: Num[A])
  extends Pattern[A] { pat =>

  override private[patterns] def aux: List[Aux] = widen :: num :: Nil

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] =
    impl.BrownImpl.expand(this)

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val loT   = t(lo)
    val hiT   = t(hi)
    val stepT = t(step)
    if (loT.eq(lo) && hiT.eq(hi) && stepT.eq(step)) this else copy(lo = loT, hi = hiT, step = stepT)
  }
}
