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

import de.sciss.patterns.Types.Top
import de.sciss.patterns.graph.impl.Truncate

/** aka `Pfin` */
final case class Take[T <: Top](in: Pat[T], length: Pat.Int)
  extends Truncate[T] {

  protected def truncate[Tx](it: Stream[Tx, T#Out], n: Int)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out] =
    it.take(n)
}

final case class Drop[T <: Top](in: Pat[T], length: Pat.Int)
  extends Truncate[T] {

  protected def truncate[Tx](it: Stream[Tx, T#Out], n: Int)(implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#Out] =
    it.drop(n)
}

final case class Stutter[T <: Top](n: Pat.Int, in: Pat[T])
  extends Pattern[T] {

  def iterator[Tx](implicit ctx: Context[Tx]): Stream[Tx, T#Out] = {
    val nIt  = n .expand
    val inIt = in.expand
    (inIt zip nIt).flatMap { case (xi, ni) => Stream.fill(ni)(xi) }
  }
}
