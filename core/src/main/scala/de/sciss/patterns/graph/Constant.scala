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

import de.sciss.patterns.Types.{Aux, CTop}

final case class Constant[T <: CTop](value: T#COut) extends Pat[T] {
  def iterator[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#COut] = Stream.continually(value)
  def embed   [Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#COut] = Stream.single     (value)

//  private[patterns] final def cClassTag: ClassTag[COut] = ClassTag(classOf[COut])

  private[patterns] def aux: List[Aux] = Nil

  private[patterns] def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, T#COut] = iterator
}