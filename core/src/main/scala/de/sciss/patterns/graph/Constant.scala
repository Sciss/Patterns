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

import de.sciss.patterns.Types.Aux

final case class Constant[A](value: A) extends Pat[A] {
  def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = Stream.continually(value)

//  def embed   [Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = Stream.single     (value)

//  private[patterns] final def cClassTag: ClassTag[COut] = ClassTag(classOf[COut])

  private[patterns] def aux: List[Aux] = Nil

//  private[patterns] def expand[Tx](implicit ctx: Context[Tx], tx: Tx): Stream[Tx, A] = iterator
//
//  private[patterns] def reset[Tx]()(implicit ctx: Context[Tx], tx: Tx): Unit = ()

  def transform[Tx](t: Transform)(implicit ctx: Context[Tx], tx: Tx): Pat[A] = this
}