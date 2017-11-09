/*
 *  Patterns.scala
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

import de.sciss.patterns.Types.{Num, Top}
import de.sciss.patterns.graph.impl.SeriesLike

/** A pattern that generates an arithmetic series. Corresponds to `Pseries` in SuperCollider,
  * but does not have a `length` arguments. Use `.take(N)` instead.
  */
final case class Series[T1 <: Top, T2 <: Top, T <: Top](start: Pat[T1], step: Pat[T2])
                                                       (implicit protected val br: Num[T1, T2, T])
  extends SeriesLike[T1, T2, T] {

  protected def op(a: T#Out, b: T#Out): T#Out = br.plus(a, b)
}

final case class Geom[T1 <: Top, T2 <: Top, T <: Top](start: Pat[T1], step: Pat[T2])
                                                     (implicit protected val br: Num[T1, T2, T])
  extends SeriesLike[T1, T2, T] {

  protected def op(a: T#Out, b: T#Out): T#Out = br.plus(a, b)
}
