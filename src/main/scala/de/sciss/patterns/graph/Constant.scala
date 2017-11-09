/*
 *  Constant.scala
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

import de.sciss.patterns.Types.TopT

final case class Constant[A, T <: TopT[A]](x: A) extends Pat[T] {
  def iterator(implicit ctx: Context): Iterator[A] = Iterator.continually(x)
  def embed   (implicit ctx: Context): Iterator[A] = Iterator.single     (x)

  private[patterns] def expand(implicit ctx: Context): Iterator[A] = iterator
}