/*
 *  SortWithItStream.scala
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
package impl

import de.sciss.lucre.stm.Base

final class SortWithItStream[S <: Base[S], A](tx0: S#Tx)(implicit ctx: Context[S])
  extends Stream[S, (A, A)] {

  private[this] val id          = tx0.newId()
  private[this] val _valid      = tx0.newBooleanVar(id, false)
  private[this] val _hasZ       = tx0.newBooleanVar(id, false)
  private[this] val _hasNext    = tx0.newBooleanVar(id, false)
  private[this] val pairInRef   = ??? : S#Var[(Vector[A], Vector[A])] // ctx.newVar[(Vector[A], Vector[A])](null)(tx0)
  private[this] val count       = tx0.newIntVar(id, 0)

  def advance(x: Vector[A], y: Vector[A])(implicit tx: S#Tx): Unit = {
    pairInRef() = (x, y)
    count()     = 0
    _hasZ()     = true
    calcHasNext()
  }

  private def calcHasNext()(implicit tx: S#Tx): Unit = {
    if (_hasZ()) {
      val (x, y) = pairInRef()
      val sz      = math.min(x.size, y.size)
      val hn      = sz > count()
      _hasNext()  = hn
    } else {
      _hasNext() = false
    }
  }

  private def validate()(implicit tx: S#Tx): Unit =
    if (!_valid()) {
      _valid()    = true
//      _hasZ()     = false
      count()     = 0
      calcHasNext()
    }

//  def resetOuter()(implicit tx: S#Tx): Unit = {
//    _valid() = false
//  }

  def reset()(implicit tx: S#Tx): Unit =
    _valid() = false

  def hasNext(implicit tx: S#Tx): Boolean = {
    validate()
    _hasZ() && _hasNext()
  }

  def next()(implicit tx: S#Tx): (A, A) = {
    if (!hasNext) Stream.exhausted()
    val (x, y)  = pairInRef()
    val sz      = math.min(x.size, y.size)
    val c0      = count()
    val res     = (x(c0), y(c0))
    val c1      = c0 + 1
    count()     = c1
    if (c1 == sz) {
      _hasZ()     = false
      _hasNext()  = false
    }
    res
  }
}
