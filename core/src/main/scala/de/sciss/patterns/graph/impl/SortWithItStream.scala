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

final class SortWithItStream[Tx, A](tx0: Tx)(implicit ctx: Context[Tx])
  extends Stream[Tx, (A, A)] {

  private[this] val _valid      = ctx.newVar(false)(tx0)
  private[this] val _hasZ       = ctx.newVar(false)(tx0)
  private[this] val _hasNext    = ctx.newVar(false)(tx0)
  private[this] val pairInRef   = ctx.newVar[(Vector[A], Vector[A])](null)(tx0)
  private[this] val count       = ctx.newVar(0)(tx0)

  def advance(x: Vector[A], y: Vector[A])(implicit tx: Tx): Unit = {
    pairInRef() = (x, y)
    count()     = 0
    _hasZ()     = true
    calcHasNext()
  }

  private def calcHasNext()(implicit tx: Tx): Unit = {
    if (_hasZ()) {
      val (x, y) = pairInRef()
      val sz      = math.min(x.size, y.size)
      val hn      = sz > count()
      _hasNext()  = hn
    } else {
      _hasNext() = false
    }
  }

  private def validate()(implicit tx: Tx): Unit =
    if (!_valid()) {
      _valid()    = true
//      _hasZ()     = false
      count()     = 0
      calcHasNext()
    }

//  def resetOuter()(implicit tx: Tx): Unit = {
//    _valid() = false
//  }

  def reset()(implicit tx: Tx): Unit =
    _valid() = false

  def hasNext(implicit tx: Tx): Boolean = {
    validate()
    _hasZ() && _hasNext()
  }

  def next()(implicit tx: Tx): (A, A) = {
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
