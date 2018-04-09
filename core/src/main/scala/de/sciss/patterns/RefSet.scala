/*
 *  Context.scala
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

import de.sciss.lucre.stm.Base

import scala.collection.immutable

trait RefSet[S <: Base[S], A] {
  def add     (elem: A)(implicit tx: S#Tx): Boolean
  def remove  (elem: A)(implicit tx: S#Tx): Boolean
  def contains(elem: A)(implicit tx: S#Tx): Boolean

  def foreach[B](f: A => B)(implicit tx: S#Tx): Unit

  def toSet(implicit tx: S#Tx): immutable.Set[A]
}
