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
}

final case class Drop[A](in: Pat[A], length: Pat[Int])
  extends Truncate[A] {

  protected def truncate[Tx](it: Stream[Tx, A], n: Int)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] =
    it.drop(n)
}

final case class Stutter[A](n: Pat[Int], in: Pat[A])
  extends Pattern[A] {

  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = {
    val nIt  = n .expand
    val inIt = in.expand
    (inIt zip nIt).flatMap { case (xi, ni) => Stream.fill(ni)(xi) }
  }
}
