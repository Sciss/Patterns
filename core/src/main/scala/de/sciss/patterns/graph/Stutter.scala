/*
 *  Stutter.scala
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

final case class Stutter[A](in: Pat[A], n: Pat[Int])
  extends Pattern[A] {

  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = {
    val inIt = in.expand
    val nIt  = n .expand
    (inIt zip nIt).flatMap { case (xi, ni) => Stream.fill(ni)(xi) }
  }

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = {
    val inT = t(in) .transform(t)
    val nT  = t(n)  .transform(t)
    if (inT.eq(in) && nT.eq(n)) this else copy(in = inT, n = nT)
  }
}