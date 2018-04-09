/*
 *  SysInMemoryMap.scala
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

package de.sciss.patterns.lucre
package impl

import de.sciss.lucre.stm.Sys
import de.sciss.patterns.RefMap

import scala.concurrent.stm.TMap

final class SysInMemoryMap[S <: Sys[S], K, V] extends RefMap[S, K, V] {
  private[this] val peer = TMap.empty[K, V]

  def put     (key: K, value: V )(implicit tx: S#Tx): Option[V]  = peer.put      (key, value) (tx.peer)
  def remove  (key: K           )(implicit tx: S#Tx): Option[V]  = peer.remove   (key)        (tx.peer)
  def contains(key: K           )(implicit tx: S#Tx): Boolean    = peer.contains (key)        (tx.peer)
  def get     (key: K           )(implicit tx: S#Tx): Option[V]  = peer.get      (key)        (tx.peer)

  def foreach[B](f: ((K, V)) => B)(implicit tx: S#Tx): Unit = peer.foreach[B](f)(tx.peer)

  def toMap(implicit tx: S#Tx): Map[K, V] = peer.snapshot
}
