/*
 *  PlainInMemoryMap.scala
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

final class PlainInMemoryMap[K, V] extends RefMap[Plain, K, V] {
  private[this] val peer = mutable.Map.empty[K, V]

  def put     (key: K, value: V )(implicit tx: Plain): Option[V]  = peer.put      (key, value)
  def remove  (key: K           )(implicit tx: Plain): Option[V]  = peer.remove   (key)
  def contains(key: K           )(implicit tx: Plain): Boolean    = peer.contains (key)
  def get     (key: K           )(implicit tx: Plain): Option[V]  = peer.get      (key)

  def foreach[B](f: ((K, V)) => B)(implicit tx: Plain): Unit = peer.foreach[B](f)

  def toMap(implicit tx: Plain): Map[K, V] = peer.toMap[K, V]
}
