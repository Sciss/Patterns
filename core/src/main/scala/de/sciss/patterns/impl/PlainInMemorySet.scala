/*
 *  PlainInMemorySet.scala
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
package impl

import de.sciss.lucre.stm.Plain

import scala.collection.mutable

final class PlainInMemorySet[A] extends RefSet[Plain, A] {
  private[this] val peer = mutable.Set.empty[A]

  def add     (elem: A)(implicit tx: Plain): Boolean = peer.add     (elem)
  def remove  (elem: A)(implicit tx: Plain): Boolean = peer.remove  (elem)
  def contains(elem: A)(implicit tx: Plain): Boolean = peer.contains(elem)

  def foreach[B](f: A => B)(implicit tx: Plain): Unit = peer.foreach(f)

  def toSet(implicit tx: Plain): Set[A] = peer.toSet[A]
}
