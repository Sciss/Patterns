/*
 *  FilterPatterns.scala
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

import de.sciss.patterns.graph.impl.Truncate

/** aka `Pfin` */
final case class Take[A](in: Pat[A], length: Pat[Int])
  extends Truncate[A] {

  protected def truncate[Tx](it: Stream[Tx, A], n: Int)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    it.take(n)

  def transform(t: Transform): Pat[A] = {
    val inT     = t(in)     .transform(t)
    val lengthT = t(length) .transform(t)
    if (inT.eq(in) && lengthT.eq(length)) this else copy(in = inT, length = lengthT)
  }
}

final case class Drop[A](in: Pat[A], length: Pat[Int])
  extends Truncate[A] {

  protected def truncate[Tx](it: Stream[Tx, A], n: Int)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    it.drop(n)

  def transform(t: Transform): Pat[A] = {
    val inT     = t(in)     .transform(t)
    val lengthT = t(length) .transform(t)
    if (inT.eq(in) && lengthT.eq(length)) this else copy(in = inT, length = lengthT)
  }
}

final case class Stutter[A](in: Pat[A], n: Pat[Int])
  extends Pattern[A] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
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