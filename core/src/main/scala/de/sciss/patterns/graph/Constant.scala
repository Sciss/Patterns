/*
 *  Constant.scala
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

import de.sciss.patterns.Types.{Aux, Top}

final case class Constant[T <: Top](value: T#Out) extends Pat[T] {
  def iterator(implicit ctx: Context): Stream[T#Out] = Stream.continually(value)
  def embed   (implicit ctx: Context): Stream[T#Out] = Stream.single     (value)

  private[patterns] def aux: List[Aux] = Nil

  private[patterns] def expand(implicit ctx: Context): Stream[T#Out] = iterator
}