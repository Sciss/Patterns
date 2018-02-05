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

import de.sciss.patterns.Types.Top

final class SortWithItStream[Tx, T <: Top](tx0: Tx)(implicit ctx: Context[Tx])
  extends Stream[Tx, (T#Out[Tx], T#Out[Tx])] {

//  private[this] val _valid      = ctx.newVar(false)
  private[this] val _hasZ       = ctx.newVar(false)
  private[this] val _hasNext    = ctx.newVar(false)

  private[this] val pairInRef   = ctx.newVar[(Vector[T#Out[Tx]], Vector[T#Out[Tx]])](null)
  private[this] val count       = ctx.newVar(0)

  def advance(x: Vector[T#Out[Tx]], y: Vector[T#Out[Tx]])(implicit tx: Tx): Unit = {
    pairInRef() = (x, y)
    count()     = 0
    _hasZ()     = true
    val sz      = math.min(x.size, y.size)
    val hn      = sz > 0
    _hasNext()  = hn
  }

  private def validate()(implicit tx: Tx): Unit = ()
//    if (!_valid()) {
//      _valid()    = true
//      _hasZ()     = false
//      val ohn     = zStream.hasNext
//      _hasZ()     = ohn
//      _hasNext()  = ohn
//      if (ohn) {
//        val zValue    = zStream.next()
//        advance(zValue)
//      }
//    }

//  def resetOuter()(implicit tx: Tx): Unit = {
//    _valid() = false
//  }

  def reset()(implicit tx: Tx): Unit = {
    ???
    //      val hi = _hasIn()
    //      logStream(s"$simpleString.reset(); hasIn = $hi")
    //      if (hi) {
    //        val inValue = inStream()
    //        inValue.reset()
    //        _hasNext() = inValue.hasNext
    //      }
  }

  def hasNext(implicit tx: Tx): Boolean = {
    validate()
    _hasZ() && _hasNext()
  }

  def next()(implicit tx: Tx): (T#Out[Tx], T#Out[Tx]) = {
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
