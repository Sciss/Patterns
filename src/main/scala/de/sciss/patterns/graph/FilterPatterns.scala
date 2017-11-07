/*
 *  FilterPatterns.scala
 *  (Patterns)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.patterns
package graph

import de.sciss.patterns.Types.{IntTop, Top}
import de.sciss.patterns.graph.impl.Truncate

/** aka `Pfin` */
final case class Take[T <: Top](in: Pat[T], length: Pat[IntTop])
  extends Truncate[T] {

  protected def truncate(it: Iterator[tpe.Out], n: Int): Iterator[tpe.Out] = it.take(n)
}

final case class Drop[T <: Top](in: Pat[T], length: Pat[IntTop])
  extends Truncate[T] {

  protected def truncate(it: Iterator[tpe.Out], n: Int): Iterator[tpe.Out] = it.drop(n)
}

