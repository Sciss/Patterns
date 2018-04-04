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

import de.sciss.lucre.stm.Base
import de.sciss.patterns.Types.Aux

final case class Constant[A](value: A) extends Pat[A] {
  def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = Stream.continually(value)

  override def toString: String = value.toString

//  def embed   [Tx](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = Stream.single     (value)

//  private[patterns] final def cClassTag: ClassTag[COut] = ClassTag(classOf[COut])

  private[patterns] def aux: List[Aux] = Nil

//  private[patterns] def expand[S <: Base[S]](implicit ctx: Context[S], tx: S#Tx): Stream[S, A] = iterator
//
//  private[patterns] def reset[Tx]()(implicit ctx: Context[S], tx: S#Tx): Unit = ()

  def transform[S <: Base[S]](t: Transform)(implicit ctx: Context[S], tx: S#Tx): Pat[A] = this
}