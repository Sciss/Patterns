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

trait RefMap[S <: Base[S], K, V] {
  def put     (key: K, value: V)(implicit tx: S#Tx): Option[V]
  def get     (key: K)(implicit tx: S#Tx): Option[V]
  def remove  (key: K)(implicit tx: S#Tx): Option[V]
  def contains(key: K)(implicit tx: S#Tx): Boolean

  def foreach[B](f: ((K, V)) => B)(implicit tx: S#Tx): Unit

  def toMap(implicit tx: S#Tx): immutable.Map[K, V]
}
