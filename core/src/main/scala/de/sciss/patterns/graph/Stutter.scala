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

final case class Stutter[A](in: Pat[A], n: Pat[Int])
  extends Pattern[A] {

  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    val inIt = in.expand
    val nIt  = n .expand
    (inIt zip nIt).flatMap { case (xi, ni) => Stream.fill(ni)(xi) }
  }

  def transform(t: Transform): Pat[A] = {
    val inT = t(in) .transform(t)
    val nT  = t(n)  .transform(t)
    if (inT.eq(in) && nT.eq(n)) this else copy(in = inT, n = nT)
  }
}