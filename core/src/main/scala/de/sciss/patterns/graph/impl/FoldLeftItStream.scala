/*
 *  FoldLeftItStream.scala
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

final class FoldLeftItStream[Tx, T1 <: Top, T <: Top](outer: Pat[Pat[T1]], z: Pat[T], tx0: Tx)
                                                     (implicit ctx: Context[Tx])
  extends Stream[Tx, (T1#Out[Tx], T#Out[Tx])] {

  private[this] lazy val simpleString = {
    val os0 = outer.toString
    val os  = if (os0.length <= 24) os0 else s"${os0.substring(0, 23)}..."
    s"FoldLeftItStream($os)"
  }

  override def toString: String = simpleString

  private[this] val outerStream: Stream[Tx, Stream[Tx, T1#Out[Tx]]] = outer.expand(ctx, tx0)
  private[this] val zStream    : Stream[Tx, T#Out[Tx]]              = z    .expand(ctx, tx0)

  private[this] val inStream    = ctx.newVar[Stream[Tx, T1#Out[Tx]]](null)
  private[this] val _valid      = ctx.newVar(false)
  private[this] val _hasZ       = ctx.newVar(false)
  private[this] val _hasNext    = ctx.newVar(false)

  private[this] val zValueRef   = ctx.newVar[T#Out[Tx]](null.asInstanceOf[T#Out[Tx]])

  def result(implicit tx: Tx): T#Out[Tx] = {
    validate()
    if (!_hasZ()) Stream.exhausted()
    zValueRef()
  }

  def advance(x: T#Out[Tx])(implicit tx: Tx): Unit = {
    zValueRef() = x
    _hasZ()     = true
    val hn      = outerStream.hasNext
    _hasNext()  = hn
    if (hn) {
      val os      = outerStream.next()
      inStream()  = os
      _hasNext()  = os.hasNext
    }
  }

  private def validate()(implicit tx: Tx): Unit =
    if (!_valid()) {
      _valid()    = true
      val ohn     = zStream.hasNext
      _hasZ()     = ohn
      _hasNext()  = ohn
      if (ohn) {
        val zValue    = zStream.next()
        advance(zValue)
      }
    }

  def resetOuter()(implicit tx: Tx): Unit = {
    logStream(s"$simpleString.resetOuter()")
    _valid() = false
  }

  def reset()(implicit tx: Tx): Unit = {
    println("FoldLeftInStream. TODO: reset")
    ()
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
    _hasNext()
  }

  def next()(implicit tx: Tx): (T1#Out[Tx], T#Out[Tx]) = {
    if (!hasNext) Stream.exhausted()
    val in      = inStream()
    val res     = (in.next(), result)
    _hasZ()     = false
    _hasNext()  = false
    res
  }
}
